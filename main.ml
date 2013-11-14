open Types
open Params

let now () = Unix.gettimeofday ()

let print_infos time graph =
  Printf.printf "%d/%d/%.3f\n"
    (Viz.G.nb_vertex graph)
    (Viz.G.nb_edges graph)
    time

let () =
  Arg.parse speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
  try
    if not !Params.quiet then begin
      print_string ("Running with: \n" ^ (string_of_configuration ()));
      print_newline ()
    end;
    let node = Parser.parse (Lexer.lex !input) in
    let start = now () in
    let res, graph = Cesk.eval node in
    let stop = now () in
    if not !Params.quiet then
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
