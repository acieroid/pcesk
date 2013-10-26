type scheme_node =
  | Identifier of string
  | String of string
  | Integer of int
  | Boolean of bool
  | List of scheme_node list


val string_of_node : scheme_node -> string
