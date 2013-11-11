open Types
open Params

let () =
  Arg.parse speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
  try
    print_string ("Running with: \n" ^ (string_of_configuration ()));
    print_newline ();
    let node = Parser.parse (Lexer.lex !input) in
    let res, graph = Cesk.eval node in
    List.iter (fun (value, env, store) ->
        print_string (string_of_value value); print_newline ())
      res;
    begin match !graph_file with
      | Some f -> Viz.output_graph f graph
      | None -> ()
    end;
    close_in !input
  with
  | e -> print_string (Exceptions.string_of_exception e)
