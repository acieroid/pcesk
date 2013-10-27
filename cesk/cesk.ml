(* TODO:
   - put the timestamp in the state instead of the store
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
type time = int
type state = {
    exp : exp;
    env : env;
    store : store;
    addr : addr;
    change : kont_op;
    time : time;
  }

(** Exceptions *)

exception NYI
exception PrimWrongArgType of string * value
exception Malformed of string * node
exception InvalidKeyword of string
exception UnboundIdentifier of string
exception UnboundAddress of addr
exception InvalidNumberOfArguments of int * int
exception NotAFunction of value
exception NotAKont of value
exception EvaluationStuck of node

(** String conversion *)

let string_of_kont = function
  | OperatorKont (_, _, a) -> "Operator(" ^ (Addr.string_of_address a) ^ ")"
  | OperandsKont (_, _, _, _, a) -> "Operands(" ^ (Addr.string_of_address a) ^ ")"
  | HaltKont -> "Halt"

let string_of_value = function
  | String s -> "\"" ^ s ^ "\""
  | Integer n -> string_of_int n
  | Boolean true -> "#t"
  | Boolean false -> "#f"
  | Symbol sym -> "'" ^ sym
  | Closure _ -> "#<closure>"
  | Primitive (name, _) -> "#<primitive " ^ name ^ ">"
  | Kont k -> "#<continuation " ^ (string_of_kont k) ^ ">"

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
  | UnboundAddress a ->
      "Unbound address: " ^ (Addr.string_of_address a)
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

let print_add a k =
  print_string ("Add: " ^
                (Addr.string_of_address a) ^ " -> " ^
                (string_of_kont k));
  print_newline ()

(** Environment *)

let empty_env = Env.empty
let env_lookup env name =
  try
    Env.lookup env name
  with
    Not_found -> raise (UnboundIdentifier name)
let env_extend env name a =
  print_string ("extend(" ^ name ^ ", " ^ (Addr.string_of_address a) ^ ")\n");
  Env.extend env name a

(** Store *)

let print_store store =
  print_string ("store(" ^
                (Store.string_of_store store
                   (fun (v, env) -> string_of_value v)) ^ ")\n")

let empty_store = Store.empty
let store_lookup store a =
  try
    Store.lookup store a
  with
    Not_found -> raise (UnboundAddress a)
let store_extend store a v =
  print_string ("store(" ^ (Addr.string_of_address a) ^ ", " ^ (string_of_value (fst v)) ^ ")\n");
  Store.update store a v

let extract_kont state = match store_lookup state.store state.addr with
| (Kont kont, _) -> kont
| (v, _) -> raise (NotAKont v)

(** Allocation *)

let alloc (state : state) : addr = Addr.alloc state.time

(** Time *)

let tick (state : state) : time = (state.time)+1

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

let install_primitives (state : state) : state =
  let inst state ((name, _) as prim) =
    let a = alloc state in
    {state with
     env = env_extend state.env name a;
     store = store_extend state.store a (Primitive prim, state.env);
     time = tick state}
  in
  List.fold_left inst state primitives

(** Keywords *)

let keywords = ["lambda"]

let is_keyword kw = List.mem kw keywords

let step_keyword (kw : string) (args : node list)
    (state : state) : state = match kw with
    | "lambda" ->
        begin match args with
        | args_node :: body :: [] ->
            begin match args_node with
            | Scheme_ast.List args_node ->
                let args = List.map (function
                  | Scheme_ast.Identifier x -> x
                  | node -> raise (Malformed ("lambda argument", node))) args_node in
                {state with
                 exp = Value (Closure ((args, body), state.env));
                 change = Epsilon;
                 time = tick state}
            | _ -> raise NYI
            end
        | _ -> raise NYI
        end
    | _ -> raise (InvalidKeyword kw)

(** State manipulation *)

let apply_function (rator : value) (rands : value list)
    (state : state) : state = match rator with
    | Closure ((ids, body), env') ->
        if List.length ids != List.length rands then
          raise (InvalidNumberOfArguments (List.length ids, List.length rands));
        let args = List.combine ids rands in
        let addrs, state = List.fold_right (fun x (addrs, state) ->
          let a = alloc state in
          ((x, a) :: addrs, {state with time = tick state}))
            args ([], state) in
        let extended_env = List.fold_left
            (fun env ((x, _), a) ->
              env_extend env x a) state.env addrs
        and extended_store = List.fold_left
            (fun store ((_, v), a) ->
              store_extend store a (v, state.env)) state.store addrs in
        { state with
          exp = Node body;
          env = extended_env;
          store = extended_store;
          change = Pop;
          time = tick state}
    | Primitive prim ->
        { state with
          exp = Value (apply_primitive prim rands);
          change = Pop;
          time = tick state}
    | _ -> raise (NotAFunction rator)

let step (state : state) : state =
  let kont = extract_kont state in
  match state.exp with
  | Node n ->
      begin match n with
      | Scheme_ast.Identifier x ->
          print_string "identifier\n";
          let (v, env') = store_lookup state.store (env_lookup state.env x) in
          { state with
            exp = Value v;
            env = env';
            change = Epsilon;
            time = tick state }
      | Scheme_ast.String s ->
          { state with
            exp = Value (String s);
            change = Epsilon;
            time = tick state }
      | Scheme_ast.Integer n ->
          { state with
            exp = Value (Integer n);
            change = Epsilon;
            time = tick state }
      | Scheme_ast.Boolean b ->
          { state with
            exp = Value (Boolean b);
            change = Epsilon;
            time = tick state }
      | Scheme_ast.List (Scheme_ast.Identifier kw :: args) when is_keyword kw ->
          print_string "keyword\n";
          step_keyword kw args state
      | Scheme_ast.List (rator :: rands) ->
          print_string ("call " ^ (string_of_int (List.length rands)));
          let kont' = OperatorKont (rands, state.env, state.addr)
          and a' = alloc state in
          print_add a' kont';
          let store' = store_extend state.store a' (Kont kont', state.env) in
          { state with
            exp = Node rator;
            store = store';
            addr = a';
            change = Push;
            time = tick state }
      | _ -> raise (EvaluationStuck n)
      end
  | Value v ->
      begin match kont with
        (* TODO: problem around here, operands seems to point to operator, while it should not (it should point to operator's kont) *)
      | OperatorKont ([], env', c) ->
          print_string ("operator1: " ^ (string_of_value v) ^ "\n");
          apply_function v [] { state with
                                env = env';
                                addr = c;
                                time = tick state }
      | OperatorKont (rand :: rands, env', c) ->
          print_string ("operator2: " ^ (string_of_value v) ^ "\n");
          let kont' = OperandsKont (v, rands, [], env', c) in
          let a' = alloc state in
          print_add a' kont';
          let store' = store_extend state.store a' (Kont kont', state.env) in
          { exp = Node rand;
            env = env';
            store = store';
            addr = a';
            change = Push;
            time = tick state }
      | OperandsKont (rator, [], values, env', c) ->
          print_string ("operands1: " ^ (string_of_value v) ^ "\n");
          let rands = List.rev (v :: values) in
          apply_function rator rands { state with
                                       env = env';
                                       addr = c;
                                       time = tick state }
      | OperandsKont (rator, rand :: rands, values, env', c) ->
          print_string ("operands2: " ^ (string_of_value v) ^ "\n");
          let kont' = OperandsKont (rator, rands, v :: values, env', c) in
          let a' = alloc state in
          print_add a' kont';
          let store' = store_extend state.store a' (Kont kont', state.env) in
          { exp = Node rand;
            env = env';
            store = store';
            addr = a';
            change = Push;
            time = tick state }
      | HaltKont -> { state with change = Epsilon; time = tick state }
      end

(** Injection *)

let empty_state = {
  exp = Value (Integer 0);
  env = empty_env;
  store = empty_store;
  addr = Addr.alloc (-1);
  change = Epsilon;
  time = 0;
}

let inject (e : node) : state =
  let state = install_primitives empty_state in
  let a_halt = alloc state in
  print_add a_halt HaltKont;
  let store' = store_extend state.store a_halt (Kont HaltKont, state.env) in
  { state with
    exp = Node e;
    store = store';
    addr = a_halt;
    time = tick state}

(** Graph representation *)

module GraphNode = struct
  type t = state
  let compare state state' =
    match (state.exp, state'.exp) with
    | Node _, Node _
    | Value _, Value _ -> Addr.compare state.addr state'.addr
    | Node _, Value _ -> 1
    | Value _, Node _ -> -1
  let hash state = Hashtbl.hash (state.exp, state.addr)
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
    let vertex_attributes (state : V.t) =
      match state.exp with
      | Node n -> [`Shape `Box; `Style `Filled; `Fillcolor 0xFFDDDD;
                   `Label (Scheme_ast.string_of_node n)]
      | Value v -> [`Shape `Box; `Style `Filled; `Fillcolor 0xDDFFDD;
                    `Label (string_of_value v)]
    let vertex_name (state : V.t) =
      match state.exp with
      | Node n -> "node_" ^ (Addr.string_of_address state.addr)
      | Value v -> "value_" ^ (Addr.string_of_address state.addr)
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end

module Dot = Graph.Graphviz.Dot(DotArg)

(** Evaluation *)

let string_of_state (state : state) : string =
  let kont = extract_kont state in
  (match state.exp with
  | Node n -> "node " ^ (Scheme_ast.string_of_node n)
  | Value v -> "value " ^ (string_of_value v)) ^
  " k:" ^ (string_of_kont kont) ^ "@" ^
  (Addr.string_of_address state.addr)

let eval (e : node) : value * env * store =
  let string_of_update state state' =
    match state'.change with
    | Epsilon -> "ε"
    | Pop -> "-" ^ (string_of_kont (extract_kont state))
    | Push -> "+" ^ (string_of_kont (extract_kont state')) in
  let rec loop state g =
    let kont = extract_kont state in
    match state.exp, kont with
    | (Value result, HaltKont) -> (result, state.env, state.store), g
    | _ ->
        let state' = step state in
        let vertex = G.V.create state
        and vertex' = G.V. create state' in
        let edge = G.E.create vertex (string_of_update state state') vertex' in
        let g' = G.add_edge_e (G.add_vertex g vertex') edge in
        print_string ((string_of_state state') ^ " -> " ^ (string_of_update state state'));
        print_newline ();
        loop state' g'
  in
  let res, graph = loop (inject e) G.empty in
  let out = open_out_bin "/tmp/foo.dot" in
  Dot.output_graph out graph;
  res
