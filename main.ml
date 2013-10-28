open Types

let _ =
  try
    let node = Scheme_parser.parse (Scheme_lexer.lex stdin) in
    print_string (Scheme_ast.string_of_node node)
    (* let (value, env, store) = Cesk.eval node in *)
    (* print_string (string_of_value value) *)
  with
  | e -> raise e (* print_string (Cesk.string_of_exception e) *)
