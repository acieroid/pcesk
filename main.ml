open Types
open Params

let now () = Unix.gettimeofday ()

let print_infos time vertex edges =
  Printf.printf "%d/%d/%.3f\n" vertex edges time

let eval node =
  let res, graph = if !Params.parallel then
      Pcesk.eval node
    else
      Cesk.eval node in
  BatOption.may (Viz.output_graph graph) !graph_file;
  close_in !input;
  (res, Viz.G.nb_vertex graph, Viz.G.nb_edges graph)

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
    let res, vertex, edges = eval node in
    let stop = now () in
    if not !Params.quiet then
      List.iter (fun (value, env, store) ->
          print_string (string_of_value value); print_newline ())
        res;
    print_infos (stop -. start) vertex edges
  with
  | e -> print_string (Exceptions.string_of_exception e)
