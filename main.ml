open Types
open Params

let now () = Unix.gettimeofday ()

let print_infos time vertex edges =
  Printf.printf "%d/%d/%.3f\n" vertex edges time

let eval node =
  if !Params.parallel then begin
    let res, graph = Pcesk.eval node in
    BatOption.may (Pviz.output_graph graph) !graph_file;
    close_in !input;
    (res, Pviz.G.nb_vertex graph, Pviz.G.nb_edges graph)
  end else begin
    let res, graph = Cesk.eval node in
    BatOption.may (Viz.output_graph graph) !graph_file;
    close_in !input;
    (res, Viz.G.nb_vertex graph, Viz.G.nb_edges graph)
  end

let run node =
  let start = now () in
  let res, vertex, edges = eval node in
  let stop = now () in
  if not !Params.quiet then
    List.iter (fun (value, env, store) ->
        print_string (string_of_value value); print_newline ())
      res;
  print_infos (stop -. start) vertex edges

let print_ast node =
  print_string (Ast.string_of_node ~tags:true node)

let () =
  Arg.parse speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
  try
    if not !Params.quiet then begin
      print_string ("Running with: \n" ^ (string_of_configuration ()));
      print_newline ()
    end;
    let action = match !Params.target with
      | Params.Run -> run
      | Params.PrintAST -> print_ast in
    let node = Parser.parse (Lexer.lex !input) in
    action node
  with
  | e -> print_string (Exceptions.string_of_exception e)
