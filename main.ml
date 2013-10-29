open Types

let _ =
  try
    let node = Scheme_parser.parse (Scheme_lexer.lex stdin) in
    let res = Cesk.eval node in
    List.iter (fun (value, env, store) ->
        print_string (string_of_value value); print_newline ())
      res
  with
  | e -> print_string (Cesk.string_of_exception e)
