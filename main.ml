open Types
open Params

let now () = Unix.gettimeofday ()

let print_infos time graph =
  print_string "Time took: "; print_float time; print_newline ();
  print_string "Nodes: "; print_int (Viz.G.nb_vertex graph); print_newline ();
  print_string "Edges: "; print_int (Viz.G.nb_edges graph); print_newline ()

let () =
  Arg.parse speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
  try
    print_string ("Running with: \n" ^ (string_of_configuration ()));
    print_newline ();
    let node = Parser.parse (Lexer.lex !input) in
    let start = now () in
    let res, graph = Cesk.eval node in
    let stop = now () in
    List.iter (fun (value, env, store) ->
        print_string (string_of_value value); print_newline ())
      res;
    begin match !graph_file with
      | Some f -> Viz.output_graph f graph
      | None -> ()
    end;
    close_in !input;
    print_infos (stop -. start) graph
  with
  | e -> print_string (Exceptions.string_of_exception e)
