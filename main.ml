open Types

let _ =
  try
    let node = Scheme_parser.parse (Scheme_lexer.lex stdin) in
    let res, graph = Cesk.eval node in
    List.iter (fun (value, env, store) ->
        print_string (string_of_value value); print_newline ())
      res;
    let out = open_out_bin "/tmp/foo.dot" in
    Cesk.Dot.output_graph out graph;
    close_out out
  with
  | e -> print_string (Cesk.string_of_exception e)
