(* TODO:
   - Multiple arguments
   - Continuations in store
   - Abstract *)

type exp =
  | Node of node
  | Value of value
and node = Scheme_ast.scheme_node
and state = exp * env * store * kont
and storable = value * env
and addr = int
and env = string -> addr
and store = (addr -> storable) * addr
and value =
  | String of string
  | Integer of int
  | Boolean of bool
  | Symbol of string
  | Closure of lam * env
  | Kont of kont
and lam = string * node
and kont =
  | FunKont of value * env * kont
  | ArgKont of node * env * kont
  | HaltKont

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

let empty_env x = failwith ("unbound identifier: " ^ x)
let env_lookup env x = env x
let env_extend env x a =
  fun x' -> if x = x' then a else env_lookup env x

let empty_store = ((fun a -> failwith ("not in store: " ^ (string_of_int a))), 0)
let store_lookup (f, _) a = f a
let store_extend (f, _) (a : addr) (s : storable) =
  ((fun a' -> if a = a' then s else f a), a+1)
let store_alloc (_, a) = a

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
        | Scheme_ast.List [Scheme_ast.Identifier x] ->
            (Value (Closure ((x, body), env)), env, store, kont)
        | _ -> failwith "Not implemented yet"
        end
    | _ -> failwith "Not implemented yet"
    end
| _ -> failwith ("Unknown keyword: " ^ kw)

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
    | Scheme_ast.List (e0 :: e1 :: []) ->
        let kont' = ArgKont (e1, env, kont) in
        (Node e0, env, store, kont')
    | _ -> failwith ("Cannot step node:" ^ (Scheme_ast.string_of_node n))
    end
| Value v ->
    begin match kont with
    | ArgKont (e, env', kont) ->
        let kont' = FunKont (v, env, kont) in
        (Node e, env', store, kont')
    | FunKont (Closure ((x, e), _), env', kont) ->
        let a = store_alloc store in
        let extended_env = env_extend env x a
        and extended_store = store_extend store a (v, env) in
        (Node e, extended_env, extended_store, kont)
    | HaltKont -> (node, env, store, kont)
    | _ -> failwith ("Cannot step value" ^ (string_of_value v))
    end

(* val inject : node -> state *)
let inject e = (Node e, empty_env, empty_store, HaltKont)

(* val eval : node -> val * env * store *)
let eval e =
  let rec loop state = match state with
  | (Value v, env, store, HaltKont) -> (v, env, store)
  | _ -> loop (step state)
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
