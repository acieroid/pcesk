open Types
open Params
open Exceptions

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
  (* TODO: using some kind of pretty printing would be better *)
  print_string (Ast.string_of_node ~tags:true node)

let mhp node = match !Params.tag1, !Params.tag2 with
  | Some t1, Some t2 ->
    if !Params.parallel then
      let e1, e2 = Ast.find_node t1 node, Ast.find_node t2 node in
      match e1, e2 with
      | Some exp1, Some exp2 ->
        let _, graph = Pcesk.eval node in
        let may = Mhp.mhp graph t1 t2 in
        print_string ("The expressions " ^
                        (Ast.string_of_node ~tags:true exp1) ^ " and " ^
                        (Ast.string_of_node ~tags:true exp2) ^ " may " ^
                        (if may then "" else "not ") ^ "happen in parallel\n")
      | _ -> raise (BadArguments
                      "at least one of the tags is incorrect (use -target ast to find them)")
    else
      raise (BadArguments
               "cannot do MHP analysis without parallelism turned on (use -p)")
  | _ -> raise (BadArguments
                  "two tags should be specified (use -target ast to find them)")

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
      | Params.PrintAST -> print_ast
      | Params.MHP -> mhp in
    let node = Parser.parse (Lexer.lex !input) in
    action node
  with
  | e -> print_string (Exceptions.string_of_exception e)
