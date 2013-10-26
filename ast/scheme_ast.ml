type scheme_node =
  | Identifier of string
  | String of string
  | Integer of int
  | Boolean of bool
  | List of scheme_node list

let rec string_of_node = function
  | Identifier s -> s
  | String s -> "\"" ^ s ^ "\""
  | Integer n -> string_of_int n
  | Boolean true -> "#t"
  | Boolean false -> "#f"
  | List l -> "(" ^ (String.concat " " (List.map string_of_node l)) ^ ")"

