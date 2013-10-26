let _ =
  print_string (Scheme_ast.string_of_value (Scheme_parser.parse (Scheme_lexer.lex stdin)))

