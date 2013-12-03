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
  | Callcc of node
  | Spawn of node
  | Join of node (* argument of join should be an atomic expression *)
  | Cas of var * node * node (* both node arguments should be atomic *)
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
  | Callcc e ->
    "(callcc " ^ (string_of_node e) ^ ")"
  | Spawn e ->
    "(spawn " ^ (string_of_node e) ^ ")"
  | Join e ->
    "(join " ^ (string_of_node e) ^ ")"
  | Cas ((v, _), e1, e2) ->
    "(cas " ^ v ^ " " ^ (string_of_node e1) ^ " " ^ (string_of_node e2) ^ ")"

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

let rec extract_tags = function
  | (Identifier _, t)
  | (String _, t)
  | (Integer _, t)
  | (Boolean _, t) -> [t]
  | (Funcall (f, args), t) ->
    t :: ((extract_tags f) @ (Util.flatmap extract_tags args))
  | (Lambda (vars, body), t) ->
    t :: ((List.map (fun (_, t) -> t) vars) @
            (Util.flatmap extract_tags body))
  | (Begin body, t) ->
    t :: (Util.flatmap extract_tags body)
  | (LetRec (bindings, body), t) ->
    t :: (Util.flatmap (fun ((_, t), n) -> t :: extract_tags n) bindings) @
      (Util.flatmap extract_tags body)
  | (If (cond, cons, alt), t) ->
    t :: (Util.flatmap extract_tags [cond; cons; alt])
  | (Set ((_, t'), exp), t) ->
    t :: t' :: (extract_tags exp)
  | (Callcc exp, t)
  | (Spawn exp, t)
  | (Join exp, t) ->
    t :: (extract_tags exp)
  | (Cas ((_, t'), eold, enew), t) ->
    t :: t' :: ((extract_tags eold) @ (extract_tags enew))
