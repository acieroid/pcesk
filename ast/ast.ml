open Util

type tag = int
type var = string * tag
type exp =
  | Identifier of string
  | String of string
  | Integer of int
  | Boolean of bool
  | Funcall of node * node list
  | Lambda of var list * node list
  | Begin of node list
  | LetRec of (var * node) list * node list
  | If of node * node * node
  | Set of var * node
and node = exp * int

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
  | LetRec (bindings, body) ->
    "(letrec (" ^ (string_of_bindings bindings) ^ ") " ^
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

and string_of_bindings bindings =
  String.concat " "
    (List.map (fun ((var, _), value) ->
         "(" ^ var ^ " " ^ (string_of_node value) ^ ")") bindings)
