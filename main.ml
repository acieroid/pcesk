open Types

let input = ref stdin
let graph_file = ref None

let usage = "usage: " ^ (Sys.argv.(0)) ^ " [-v] [-i input] [-g graph_output]"

let speclist = [
  ("-v", Arg.Set Params.verbose,
   ": verbose mode (disable by default)");
  ("-i", Arg.String (fun s -> input := open_in s),
   ": input file (stdin by default)");
  ("-g", Arg.String (fun s -> graph_file := Some s),
   ": output file for the generated graph (nothing by default)");
]

let _ =
  Arg.parse speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
  try
    let node = Scheme_parser.parse (Scheme_lexer.lex !input) in
    (*let res, graph = Cesk.eval node in
    List.iter (fun (value, env, store) ->
        print_string (string_of_value value); print_newline ())
      res;
    begin match !graph_file with
      | Some f -> Viz.output_graph f graph
      | None -> ()
    end;*)
    print_string (Scheme_ast.string_of_node node); print_newline ();
    close_in !input
  with
  | e -> print_string (Exceptions.string_of_exception e)
