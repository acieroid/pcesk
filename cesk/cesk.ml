(* TODO:
   - Continuations in store
   - Factor
   - Abstract *)

open Concrete_env
open Concrete_store
open Concrete_addr
open Storable

module Addr = Concrete_addr
module Env = Concrete_env(Addr)
module Store = Concrete_store(Addr)

type env = Env.t
and store = (value * env) Store.t
and addr = Addr.t
and exp =
  | Node of node
  | Value of value
and node = Scheme_ast.scheme_node
and state = exp * env * store_wrap * addr
and storable = value * env
and store_wrap = store * addr
and prim = string * (value list -> value)
and value =
  | String of string
  | Integer of int
  | Boolean of bool
  | Symbol of string
  | Closure of lam * env
  | Primitive of prim
  | Kont of kont
and lam = string list * node
and kont =
  | OperatorKont of node list * env * addr
  | OperandsKont of value * node list * value list * env * addr
  | HaltKont

exception PrimWrongArgType of string * value

let string_of_value = function
  | String s -> "\"" ^ s ^ "\""
  | Integer n -> string_of_int n
  | Boolean true -> "#t"
  | Boolean false -> "#f"
  | Symbol sym -> "'" ^ sym
  | Closure _ -> "#<closure>"
  | Primitive (name, _) -> "#<primitive " ^ name ^ ">"
  | Kont _ -> "#<continuation>"

let string_of_state ((exp, env, store, kont) : state) : string = match exp with
| Node n -> "node " ^ (Scheme_ast.string_of_node n)
| Value v -> "value " ^ (string_of_value v)

let empty_env = Env.empty
let env_lookup = Env.lookup
let env_extend = Env.extend

let empty_store = (Store.empty, Addr.first)
let store_lookup (store, _) a = Store.lookup store a
let store_extend (store, _) a s = (Store.update store a s, Addr.next a)
let store_alloc (f, (a : Addr.t)) = (a, (f, Addr.next a))

let keywords = ["lambda"]

let is_keyword kw = List.mem kw keywords


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

(* val step_keyword : string -> node list -> env -> store -> addr -> state *)
let step_keyword (kw : string) (args : node list)
    (env : env) (store : store_wrap) (a : addr) : state = match kw with
    | "lambda" ->
        begin match args with
        | args_node :: body :: [] ->
            begin match args_node with
            | Scheme_ast.List args_node ->
                let args = List.map (function
                  | Scheme_ast.Identifier x -> x
                  | node -> failwith ("Malformed lambda argument: " ^
                                      (Scheme_ast.string_of_node node))) args_node in
                (Value (Closure ((args, body), env)), env, store, a)
            | _ -> failwith "Not implemented yet"
            end
        | _ -> failwith "Not implemented yet"
        end
    | _ -> failwith ("Unknown keyword: " ^ kw)

(* val apply_function : value -> value list -> env -> store -> addr -> state *)
let apply_function (rator : value) (rands : value list)
    (env : env) (store : store_wrap) (a : addr) : state = match rator with
    | Closure ((ids, body), env') ->
        if List.length rands != List.length ids then
          failwith (Printf.sprintf "Invalid number of arguments: got %d, expected %d"
                      (List.length rands) (List.length ids));
        let args = List.combine ids rands in
        let addrs, store = List.fold_right (fun x (addrs, store) ->
          let a, store' = store_alloc store in
          ((x, a) :: addrs, store')) args ([], store) in
        let extended_env = List.fold_left
            (fun env ((x, _), a) -> env_extend env x a) env addrs
        and extended_store = List.fold_left
            (fun store ((_, v), a) -> store_extend store a (v, env)) store addrs in
        (Node body, extended_env, extended_store, a)
    | Primitive prim ->
        (Value (apply_primitive prim rands), env, store, a)
    | _ -> failwith ("Not a function: " ^ (string_of_value rator))

(* val step : state -> state *)
let step ((node, env, store, a) : state) : state =
  let kont = match store_lookup store a with
  | (Kont kont, _) -> kont
  | (v, _) -> failwith ("Not a continuation: " ^ (string_of_value v)) in
  match node with
  | Node n ->
      begin match n with
      | Scheme_ast.Identifier x ->
          let (v, env') = store_lookup store (env_lookup env x) in
          (Value v, env', store, a)
      | Scheme_ast.String s ->
          (Value (String s), env, store, a)
      | Scheme_ast.Integer n ->
          (Value (Integer n), env, store, a)
      | Scheme_ast.Boolean b ->
          (Value (Boolean b), env, store, a)
      | Scheme_ast.List (Scheme_ast.Identifier kw :: args) when is_keyword kw ->
          step_keyword kw args env store a
      | Scheme_ast.List (rator :: rands) ->
          let kont' = OperatorKont (rands, env, a) in
          let (a', store') = store_alloc store in
          let store'' = store_extend store' a' (Kont kont', env) in
          (Node rator, env, store'', a')
      | _ -> failwith ("Cannot step node:" ^ (Scheme_ast.string_of_node n))
      end
  | Value v ->
      begin match kont with
      | OperatorKont ([], env', c) ->
          apply_function v [] env' store c
      | OperatorKont (rand :: rands, env', c) ->
          let kont' = OperandsKont (v, rands, [], env, c) in
          let (a', store') = store_alloc store in
          let store'' = store_extend store' a' (Kont kont', env) in
          (Node rand, env', store'', a')
      | OperandsKont (rator, [], values, env', c) ->
          let rands = List.rev (v :: values) in
          apply_function rator rands env' store c
      | OperandsKont (rator, rand :: rands, values, env', c) ->
          let kont' = OperandsKont (rator, rands, v :: values, env', c) in
          let (a', store') = store_alloc store in
          let store'' = store_extend store' a' (Kont kont', env) in
          (Node rand, env', store'', a')
      | HaltKont -> (node, env, store, a)
      end

let inject (e : node) : state =
  let (env, store) = install_primitives empty_env empty_store in
  let (a_halt, store') = store_alloc store in
  let store'' = store_extend store' a_halt (Kont HaltKont, env) in
  (Node e, env, store'', a_halt)

let eval (e : node) : value * env * store_wrap =
  let rec loop ((v, env, store, a) as state) =
    let kont = match store_lookup store a with
    | (Kont k, _) -> k
    | (v, _) -> failwith ("Not a continuation: " ^ (string_of_value v)) in
    match v, kont with
    | (Value result, HaltKont) -> (result, env, store)
    | _ ->
        let state' = step state in
        print_string ((string_of_state state') ^ "\n");
        loop state'
  in
  loop (inject e)
