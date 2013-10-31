open Types

let _ =
  try
    let node = Scheme_parser.parse (Scheme_lexer.lex stdin) in
    let res, graph = Cesk.eval node in
    List.iter (fun (value, env, store) ->
        print_string (string_of_value value); print_newline ())
      res;
    Viz.output_graph "/tmp/foo.dot" graph
  with
  | e -> print_string (Exceptions.string_of_exception e)
