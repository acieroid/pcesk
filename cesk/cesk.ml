
type state = scheme_node * env * store * addr
type env = var * addr
type store = addr * scheme_value
type value =
  | String of string
  | Integer of int
  | Boolean of bool
  | Symbol of string
  | Closure of lam * env
  | Kont of kont
type kont =
  | LetKont of value * env * scheme_node * kont
  | HaltKont

exception PrimWrongArgType of string * Scheme_ast.scheme_node

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
  | node -> failwith ("Not an atomic node: " ^ (Scheme_ast.string_of_node prim)
