type scheme_exp =
  | Identifier of string
  | String of string
  | Integer of int
  | Boolean of bool
  | List of scheme_node list
and scheme_node = scheme_exp * int

val string_of_exp : scheme_exp -> string
val string_of_node : scheme_node -> string

val compare_node : scheme_node -> scheme_node -> int
