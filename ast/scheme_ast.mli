type scheme_value =
  | Identifier of string
  | Symbol of string
  | String of string
  | Integer of int
  | Boolean of bool
  | Pair of scheme_value * scheme_value
  | Null

val string_of_value : scheme_value -> string
