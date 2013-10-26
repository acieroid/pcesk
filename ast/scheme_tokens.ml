type scheme_token =
  | EOF
  | LPAR
  | RPAR
  | QUOTE
  | BOOLEAN of bool
  | STRING of string
  | INTEGER of int
  | IDENTIFIER of string
