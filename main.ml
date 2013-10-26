let _ =
  let node = Scheme_parser.parse (Scheme_lexer.lex stdin) in
  print_string (Scheme_ast.string_of_node node)

