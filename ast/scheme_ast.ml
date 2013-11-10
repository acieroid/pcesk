open Util

type tag = int
type var = string * tag
type scheme_exp =
  | Identifier of string
  | String of string
  | Integer of int
  | Boolean of bool
  | Funcall of scheme_node * scheme_node list
  | Lambda of var list * scheme_node list
  | Begin of scheme_node list
  | Define of var * scheme_node
  | DefineFun of var * var list * scheme_node list
  | If of scheme_node * scheme_node * scheme_node
  | Set of var * scheme_node
and scheme_node = scheme_exp * int

let rec string_of_exp = function
  | Identifier s -> s
  | String s -> "\"" ^ s ^ "\""
  | Integer n -> string_of_int n
  | Boolean true -> "#t"
  | Boolean false -> "#f"
  | Funcall (f, args) ->
    "(" ^ (string_of_node f) ^ " " ^ (string_of_nodes " " args) ^ ")"
  | Lambda (args, body) ->
    "(lambda (" ^ (string_of_vars " " args) ^ ") " ^
      (string_of_nodes " " body) ^ ")"
  | Begin [] -> "(begin)"
  | Begin body ->
    "(begin " ^ (string_of_nodes " " body) ^ ")"
  | Define ((name, _), value) ->
    "(define " ^ name ^ " " ^ (string_of_node value) ^ ")"
  | DefineFun ((name, _), args, body) ->
    "(define (" ^ name ^ " " ^ (string_of_vars " " args) ^ ") " ^
      (string_of_nodes " " body) ^ ")"
  | If (cond, cons, alt) ->
    "(if " ^ (string_of_node cond) ^ " " ^
      (string_of_node cons) ^ " " ^
        (string_of_node alt) ^ ")"
  | Set ((v, _), e) ->
    "(set! " ^ v ^ " " ^ (string_of_node e) ^ ")"

and string_of_node (exp, tag) =
  (string_of_exp exp) (* ^ "@" ^ (string_of_int tag) *)

and string_of_nodes sep nodes =
  String.concat sep (List.map string_of_node nodes)

and string_of_vars sep vars =
    String.concat sep (List.map fst vars)

let rec free_variables (node, _) = match node with
  | Identifier s -> StringSet.singleton s
  | String _ | Integer _ | Boolean _ -> StringSet.empty
  | Funcall (f, args) ->
    List.fold_left StringSet.union (free_variables f)
      (List.map free_variables args)
  | Lambda (vars, body) ->
    let free_in_body = List.fold_left StringSet.union StringSet.empty
        (List.map free_variables body) in
    StringSet.diff free_in_body (string_set_of_vars vars)
  | Begin body ->
    List.fold_left StringSet.union StringSet.empty
      (List.map free_variables body)
  | Define ((name, _), value) ->
    StringSet.diff (free_variables value) (StringSet.singleton name)
  | DefineFun ((name, _), args, body) ->
    StringSet.diff (free_variables (Lambda (args, body), 0))
      (StringSet.singleton name)
  | If (cond, cons, alt) ->
    StringSet.union (free_variables cond)
      (StringSet.union (free_variables cons) (free_variables alt))
  | Set (_, value) -> free_variables value
