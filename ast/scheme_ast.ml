type scheme_exp =
  | Identifier of string
  | String of string
  | Integer of int
  | Boolean of bool
  | List of scheme_node list
and scheme_node = scheme_exp * int

let rec string_of_exp = function
  | Identifier s -> s
  | String s -> "\"" ^ s ^ "\""
  | Integer n -> string_of_int n
  | Boolean true -> "#t"
  | Boolean false -> "#f"
  | List l -> "(" ^ (String.concat " " (List.map string_of_node l)) ^ ")"

and string_of_node (exp, tag) =
  (string_of_exp exp) (* ^ "@" ^ (string_of_int tag) *)
