(* TODO:
   - Improve store and env (use maps instead of functions)
   - Continuations in store
   - Primitives
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
and state = exp * env * store_wrap * kont
and storable = value * env
and store_wrap = store * addr
and value =
  | String of string
  | Integer of int
  | Boolean of bool
  | Symbol of string
  | Closure of lam * env
  | Kont of kont
and lam = string list * node
and kont =
  | OperatorKont of node list * env * kont
  | OperandsKont of value * node list * value list * env * kont
  | HaltKont

module MyStorable : STORABLE =
  struct
    type t = value * env
  end

exception PrimWrongArgType of string * Scheme_ast.scheme_node

(* val string_of_value : value -> string *)
let string_of_value = function
  | String s -> "\"" ^ s ^ "\""
  | Integer n -> string_of_int n
  | Boolean true -> "#t"
  | Boolean false -> "#f"
  | Symbol sym -> "'" ^ sym
  | Closure _ -> "#<closure>"
  | Kont _ -> "#<continuation>"

let string_of_state (exp, env, store, kont) = match exp with
| Node n -> "node " ^ (Scheme_ast.string_of_node n)
| Value v -> "value " ^ (string_of_value v)

let empty_env = Env.empty
let env_lookup = Env.lookup
let env_extend = Env.extend

let empty_store = (Store.empty, Addr.first)
let store_lookup (store, _) a = Store.lookup store a
let store_extend (store, _) a s = (Store.update store a s, Addr.next a)
let store_alloc (f, (a : Addr.t)) = (a, (f, Addr.next a))

(* val keywords : string list *)
let keywords = ["lambda"]

(* val is_keyword : string -> bool *)
let is_keyword kw = List.mem kw keywords

(* val step_keyword : string -> node list -> env -> store -> kont -> state *)
let step_keyword kw args env store kont = match kw with
| "lambda" ->
    begin match args with
    | args_node :: body :: [] ->
        begin match args_node with
        | Scheme_ast.List args_node ->
            let args = List.map (function
              | Scheme_ast.Identifier x -> x
              | node -> failwith ("Malformed lambda argument: " ^
                                  (Scheme_ast.string_of_node node))) args_node in
            (Value (Closure ((args, body), env)), env, store, kont)
        | _ -> failwith "Not implemented yet"
        end
    | _ -> failwith "Not implemented yet"
    end
| _ -> failwith ("Unknown keyword: " ^ kw)

(* val apply_function : value -> value list -> env -> store -> kont *)
let apply_function rator rands env store kont = match rator with
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
    (Node body, extended_env, extended_store, kont)
| _ -> failwith ("Not a function: " ^ (string_of_value rator))

(* val step : state -> state *)
let step (node, env, store, kont) : state = match node with
| Node n ->
    begin match n with
    | Scheme_ast.Identifier x ->
        let (v, env') = store_lookup store (env_lookup env x) in
        (Value v, env', store, kont)
    | Scheme_ast.String s ->
        (Value (String s), env, store, kont)
    | Scheme_ast.Integer n ->
        (Value (Integer n), env, store, kont)
    | Scheme_ast.Boolean b ->
        (Value (Boolean b), env, store, kont)
    | Scheme_ast.List (Scheme_ast.Identifier kw :: args) when is_keyword kw ->
        step_keyword kw args env store kont
    | Scheme_ast.List (rator :: rands) ->
        let kont' = OperatorKont (rands, env, kont) in
        (Node rator, env, store, kont')
    | _ -> failwith ("Cannot step node:" ^ (Scheme_ast.string_of_node n))
    end
| Value v ->
    begin match kont with
    | OperatorKont ([], env', kont) ->
        apply_function v [] env' store kont
    | OperatorKont (rand :: rands, env', kont) ->
        let kont' = OperandsKont (v, rands, [], env, kont) in
        (Node rand, env', store, kont')
    | OperandsKont (rator, [], values, env', kont) ->
        let rands = List.rev (v :: values) in
        apply_function rator rands env' store kont
    | OperandsKont (rator, rand :: rands, values, env', kont) ->
        let kont' = OperandsKont (rator, rands, v :: values, env', kont) in
        (Node rand, env', store, kont')
    | HaltKont -> (node, env, store, kont)
    | _ -> failwith ("Cannot step value" ^ (string_of_value v))
    end

(* val inject : node -> state *)
let inject e = (Node e, empty_env, empty_store, HaltKont)

(* val eval : node -> val * env * store *)
let eval e =
  let rec loop state = match state with
  | (Value v, env, store, HaltKont) -> (v, env, store)
  | _ ->
      let state' = step state in
      print_string ((string_of_state state') ^ "\n");
      loop state'
  in
  loop (inject e)

(*
(* val primitives : string * (value list -> value) list *)
let primitives =
  [("+",
    fun args ->
      List.fold_left (fun arg res -> match arg with
      | Integer n -> arg + res
      | _ -> raise PrimWrongArgType ("+", arg))
        args)]

(* val is_primitive : string -> bool *)
let is_primitive prim =
  List.mem_assoc prim primitives

(* val apply_primitive : string -> value list -> value *)
let apply_primitive prim args =
  let impl = List.assoc prim primitives in
  impl args

(* val eval_atomic : scheme_node -> env -> store -> scheme_value *)
let eval_atomic v env store = match v with
  | Scheme_ast.String s -> String s
  | Scheme_ast.Integer n -> Integer n
  | Scheme_ast.Boolean b -> Boolean b
  | Scheme_ast.Pair (Identifier prim, Scheme_ast.List args) ->
      if is_primitive prim then
        apply_primitive prim
          (List.map (fun arg -> eval_atomic arg env store) args)
      else
        failwith ("Not a primitive: " ^ (Scheme_ast.string_of_node prim))
  | node -> failwith ("Not an atomic node: " ^ (Scheme_ast.string_of_node prim))
*)
