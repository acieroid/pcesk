open Types

let _ =
  try
    let node = Scheme_parser.parse (Scheme_lexer.lex stdin) in
    let (value, env, store) = Cesk.eval node in
    print_string (string_of_value value)
  with
  | e -> print_string (Cesk.string_of_exception e)
