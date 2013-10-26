type scheme_value =
  | Identifier of string
  | Symbol of string
  | String of string
  | Integer of int
  | Boolean of bool
  | Pair of scheme_value * scheme_value
  | Null

let rec string_of_value = function
  | Identifier s -> s
  | Symbol s -> "'" ^ s
  | String s -> "\"" ^ s ^ "\""
  | Integer n -> string_of_int n
  | Boolean true -> "#t"
  | Boolean false -> "#f"
  | Pair (car, cdr) as l -> "(" ^ (string_of_list l)
  | Null -> "()"
and string_of_list = function
  | Pair (car, Pair (cdr, rest)) ->
      (string_of_value car) ^ " " ^ (string_of_list (Pair (cdr, rest)))
  | Pair (car, Null) ->
      (string_of_value car) ^ ")"
  | Pair (car, cdr) ->
      (string_of_value car) ^ " . " ^ (string_of_value cdr) ^ ")"
  | x -> string_of_value x 
