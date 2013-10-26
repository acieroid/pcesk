(* TODO:
   - Abstract
   - add more stuff: define, set!, if, let/let*/letrec
 *)

open Concrete_env
open Concrete_store
open Concrete_addr
open Storable

(** Modules *)

module Addr = Concrete_addr
module Env = Concrete_env(Addr)
module Store = Concrete_store(Addr)

(** Types *)

type env = Env.t
type addr = Addr.t
type node = Scheme_ast.scheme_node
type lam = string list * node
type value =
  | String of string
  | Integer of int
  | Boolean of bool
  | Symbol of string
  | Closure of lam * env
  | Primitive of prim
  | Kont of kont
and storable = value * env
and kont =
  | OperatorKont of node list * env * addr
  | OperandsKont of value * node list * value list * env * addr
  | HaltKont
and prim = string * (value list -> value)
type exp =
  | Node of node
  | Value of value
type kont_op =
  | Push
  | Pop
  | Epsilon
type store = (value * env) Store.t
type store_wrap = store * addr
type state = exp * env * store_wrap * addr * kont_op

(** Exceptions *)

exception NYI
exception PrimWrongArgType of string * value
exception Malformed of string * node
exception InvalidKeyword of string
exception UnboundIdentifier of string
exception InvalidNumberOfArguments of int * int
exception NotAFunction of value
exception NotAKont of value
exception EvaluationStuck of node

(** String conversion *)

let string_of_value = function
  | String s -> "\"" ^ s ^ "\""
  | Integer n -> string_of_int n
  | Boolean true -> "#t"
  | Boolean false -> "#f"
  | Symbol sym -> "'" ^ sym
  | Closure _ -> "#<closure>"
  | Primitive (name, _) -> "#<primitive " ^ name ^ ">"
  | Kont _ -> "#<continuation>"

let string_of_state ((exp, env, store, kont, op) : state) : string = match exp with
| Node n -> "node " ^ (Scheme_ast.string_of_node n)
| Value v -> "value " ^ (string_of_value v)

let string_of_kont = function
  | OperatorKont _ -> "Operator"
  | OperandsKont _ -> "Operands"
  | HaltKont -> "Halt"

let string_of_exception = function
  | NYI ->
      "Not yet implemented"
  | PrimWrongArgType (s, v) ->
      "Wrong argument type to primitive '" ^ s ^ "': " ^ (string_of_value v)
  | Malformed (s, n) ->
      "Malformed " ^ s ^ ": " ^ (Scheme_ast.string_of_node n)
  | InvalidKeyword k ->
      "Invalid keyword: " ^ k
  | UnboundIdentifier s ->
      "Unbound identifier: " ^ s
  | InvalidNumberOfArguments (expected, got) ->
      "Invalid number of arguments: expected " ^ (string_of_int expected) ^
      ", got " ^ (string_of_int got)
  | NotAFunction v ->
      "Not a function: " ^ (string_of_value v)
  | NotAKont v ->
      "Not a continuation: " ^ (string_of_value v)
  | EvaluationStuck n ->
      "Evaluation is stuck at node " ^ (Scheme_ast.string_of_node n)
  | e -> raise e

(** Environment *)

let empty_env = Env.empty
let env_lookup = Env.lookup
let env_extend = Env.extend

(** Store *)

let empty_store = (Store.empty, Addr.first)
let store_lookup (store, _) a =
  print_string ("lookup(" ^ (Addr.string_of_address a) ^ "," ^
                (Store.string_of_store store (fun (v, env) -> string_of_value v)) ^ ")\n");
  Store.lookup store a
let store_extend (store, _) a s = (Store.update store a s, Addr.next a)
let store_alloc (f, (a : Addr.t)) = (a, (f, Addr.next a))

let extract_kont store a = match store_lookup store a with
| (Kont kont, _) -> kont
| (v, _) -> raise (NotAKont v)

(** Primitives *)

let primitives : (string * (value list -> value)) list =
  [("+",
    fun args ->
      let ns = List.map (fun arg -> match arg with
      | Integer n -> n
      | _ -> raise (PrimWrongArgType ("+", arg))) args in
      Integer (List.fold_left (+) 0 ns))]

let apply_primitive ((name, f) : prim) (args : value list) : value =
  f args

let install_primitives (env : env) (store : store_wrap) : (env * store_wrap) =
  let inst (env, store)  ((name, _) as prim) =
    let (a, store') = store_alloc store in
    (env_extend env name a,
     store_extend store a (Primitive prim, env)) in
  List.fold_left inst (env, store) primitives

(** Keywords *)

let keywords = ["lambda"]

let is_keyword kw = List.mem kw keywords

let step_keyword (kw : string) (args : node list)
    (env : env) (store : store_wrap) (a : addr) : state = match kw with
    | "lambda" ->
        begin match args with
        | args_node :: body :: [] ->
            begin match args_node with
            | Scheme_ast.List args_node ->
                let args = List.map (function
                  | Scheme_ast.Identifier x -> x
                  | node -> raise (Malformed ("lambda argument", node))) args_node in
                (Value (Closure ((args, body), env)), env, store, a, Epsilon)
            | _ -> raise NYI
            end
        | _ -> raise NYI
        end
    | _ -> raise (InvalidKeyword kw)

(** State manipulation *)

let apply_function (rator : value) (rands : value list)
    (env : env) (store : store_wrap) (a : addr) : state = match rator with
    | Closure ((ids, body), env') ->
        if List.length ids != List.length rands then
          raise (InvalidNumberOfArguments (List.length ids, List.length rands));
        let args = List.combine ids rands in
        let addrs, store = List.fold_right (fun x (addrs, store) ->
          let a, store' = store_alloc store in
          ((x, a) :: addrs, store')) args ([], store) in
        let extended_env = List.fold_left
            (fun env ((x, _), a) -> env_extend env x a) env addrs
        and extended_store = List.fold_left
            (fun store ((_, v), a) -> store_extend store a (v, env)) store addrs in
        (Node body, extended_env, extended_store, a, Pop)
    | Primitive prim ->
        (Value (apply_primitive prim rands), env, store, a, Pop)
    | _ -> raise (NotAFunction rator)

let step ((node, env, store, a, _) : state) : state =
  let kont = extract_kont store a in
  match node with
  | Node n ->
      begin match n with
      | Scheme_ast.Identifier x ->
          let (v, env') = store_lookup store (env_lookup env x) in
          (Value v, env', store, a, Epsilon)
      | Scheme_ast.String s ->
          (Value (String s), env, store, a, Epsilon)
      | Scheme_ast.Integer n ->
          (Value (Integer n), env, store, a, Epsilon)
      | Scheme_ast.Boolean b ->
          (Value (Boolean b), env, store, a, Epsilon)
      | Scheme_ast.List (Scheme_ast.Identifier kw :: args) when is_keyword kw ->
          step_keyword kw args env store a
      | Scheme_ast.List (rator :: rands) ->
          let kont' = OperatorKont (rands, env, a) in
          let (a', store') = store_alloc store in
          let store'' = store_extend store' a' (Kont kont', env) in
          (Node rator, env, store'', a', Push)
      | _ -> raise (EvaluationStuck n)
      end
  | Value v ->
      begin match kont with
      | OperatorKont ([], env', c) ->
          apply_function v [] env' store c
      | OperatorKont (rand :: rands, env', c) ->
          let kont' = OperandsKont (v, rands, [], env', c) in
          let (a', store') = store_alloc store in
          let store'' = store_extend store' a' (Kont kont', env) in
          (Node rand, env', store'', a', Push)
      | OperandsKont (rator, [], values, env', c) ->
          let rands = List.rev (v :: values) in
          apply_function rator rands env' store c
      | OperandsKont (rator, rand :: rands, values, env', c) ->
          let kont' = OperandsKont (rator, rands, v :: values, env', c) in
          let (a', store') = store_alloc store in
          let store'' = store_extend store' a' (Kont kont', env) in
          (Node rand, env', store'', a', Push)
      | HaltKont -> (node, env, store, a, Epsilon)
      end

(** Injection *)

let inject (e : node) : state =
  let (env, store) = install_primitives empty_env empty_store in
  let (a_halt, store') = store_alloc store in
  let store'' = store_extend store' a_halt (Kont HaltKont, env) in
  (Node e, env, store'', a_halt, Epsilon)

(** Graph representation *)

module GraphNode = struct
  type t = state
  let compare (exp, _, (_, a), _,  _) (exp', _, (_, a'), _, _) =
    match (exp, exp') with
    | Node _, Node _
    | Value _, Value _ -> Addr.compare a a'
    | Node _, Value _ -> 1
    | Value _, Node _ -> -1
  let hash (exp, _, (_, a), _, _) = Hashtbl.hash (exp, a)
  let equal = (=)
end

module GraphEdge = struct
  type t = string
  let compare = Pervasives.compare
  let equal = (=)
  let default = ""
end

module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(GraphNode)(GraphEdge)

module DotArg =
  struct
    include G
    let edge_attributes ((a, e, b) : E.t) = [`Label e]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes ((exp, _, _, _, _) : V.t) =
      match exp with
      | Node n -> [`Shape `Box; `Style `Filled; `Fillcolor 0xFFDDDD;
                   `Label (Scheme_ast.string_of_node n)]
      | Value v -> [`Shape `Box; `Style `Filled; `Fillcolor 0xDDFFDD;
                    `Label (string_of_value v)]
    let vertex_name ((exp, _, (_, a), _, _) : V.t) =
      match exp with
      | Node n -> "node_" ^ (Addr.string_of_address a)
      | Value v -> "value_" ^ (Addr.string_of_address a)
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end

module Dot = Graph.Graphviz.Dot(DotArg)

(** Evaluation *)

let eval (e : node) : value * env * store_wrap =
  let string_of_update (_, _, store, a, _) (_, _, store', a', change) =
    match change with
    | Epsilon -> "ε"
    | Pop -> "-" ^ (string_of_kont (extract_kont store a))
    | Push -> "+" ^ (string_of_kont (extract_kont store' a)) in
  let rec loop ((v, env, store, a, _) as state) g =
    let kont = extract_kont store a in
    match v, kont with
    | (Value result, HaltKont) -> (result, env, store), g
    | _ ->
        let state' = step state in
        let vertex = G.V.create state
        and vertex' = G.V. create state' in
        let edge = G.E.create vertex (string_of_update state state') vertex' in
        let g' = G.add_edge_e (G.add_vertex g vertex') edge in
        loop state' g'
  in
  let res, graph = loop (inject e) G.empty in
  let out = open_out_bin "/tmp/foo.dot" in
  Dot.output_graph out graph;
  res
