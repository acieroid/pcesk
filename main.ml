open Types
open Params
open Exceptions

let now () = Unix.gettimeofday ()

let print_infos time vertex edges =
  Printf.printf "%d/%d/%.3f\n" vertex edges time

let eval node =
  if !Params.parallel then begin
    let res, graph =
      if !Params.cpor then
        Cpor.eval node
      else
        Pcesk.eval node in
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
  print_string (Ast.string_of_node ~tags:true node);
  print_newline ()

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

let detect_conflicts ?handle_cas:(handle_cas=true) node =
  if !Params.parallel then
    let _, graph = Pcesk.eval node in
    let conflicts = Conflict.conflicts ~handle_cas graph in
    match conflicts with
    | [] -> print_string "No conflicts detected\n"
    | [(t1,t2)] ->
      begin match Ast.find_node t1 node, Ast.find_node t2 node with
      | Some e1, Some e2 ->
        print_string ("One conflict detected between the following expressions:\n" ^
                      (Ast.string_of_node ~tags:true e1) ^ ", " ^
                      (Ast.string_of_node ~tags:true e2) ^ "\n")
      | _ ->
        print_string "Conflict detected between unknown nodes! (should not happen)"
      end
    | l ->
      print_string (string_of_int (List.length l) ^
                    " conflicts detected between the following pairs of expressions:\n");
      List.iter (fun (t1, t2) ->
          match Ast.find_node t1 node, Ast.find_node t2 node with
          | Some e1, Some e2 ->
            print_string ((Ast.string_of_node ~tags:true e1) ^ ", " ^
                          (Ast.string_of_node ~tags:true e2) ^ "\n")
          | _ ->
            print_string "Conflict detected between unknown nodes! (should not happen)")
        l
  else
    raise (BadArguments
           "cannot do conflict detection without parallelism turned on (use -p)")

let detect_deadlocks node =
  if !Params.parallel then
    let _, graph = Pcesk.eval node in
    let deadlocks = Deadlock.deadlocks graph in
    match deadlocks with
    | [] -> print_string "No deadlocks detected\n"
    | l ->
      print_string ((string_of_int (List.length l)) ^
                    " possible deadlocks detected, starting at the following expressions:\n");
      List.iter (fun (tid, tag) ->
          match Ast.find_node tag node with
          | Some exp ->
            print_string (Ast.string_of_node ~tags:true exp);
            print_newline ();
            print_string ("(on tid " ^ (string_of_tid tid) ^ ")\n");
          | None ->
            print_string "Unknown node (should not happen)\n")
        l
  else
    raise (BadArguments
            "cannot do deadlock detection without parallelism turned on (use -p)")

let detect_unretried_cas node =
  if !Params.parallel then
    let _, graph = Pcesk.eval node in
    let unretried = Unretried_cas.unretried_cas graph in
    match unretried with
    | [] -> print_string "No unretried cas detected\n"
    | l ->
      print_string ((string_of_int (List.length l)) ^
                    " unretried cas found:\n");
      List.iter (fun tag ->
          match Ast.find_node tag node with
          | Some exp ->
            print_string (Ast.string_of_node ~tags:true exp);
            print_newline ()
          | None ->
            print_string "Unknown node (should not happen)\n")
        l
  else
    raise (BadArguments
             "cannot do unretried cas analysis without parallelism turned on (use -p)")

let compare_states node = match !Params.tag1, !Params.tag2 with
  | Some t1, Some t2 ->
    begin if !Params.parallel then
      let _, graph = Pcesk.eval node in
      let s1, s2 = Pviz.find_node graph t1, Pviz.find_node graph t2 in
      match s1, s2 with
      | Some state1, Some state2 -> Pcesk_types.print_difference state1 state2
      | _ -> raise (BadArguments
                     ("at least one of the state ids is incorrect " ^
                        "(use -v 2 and look in the produced graph to find them"))
    else
      let _, graph = Cesk.eval node in
      let s1, s2 = Viz.find_node graph t1, Viz.find_node graph t2 in
      match s1, s2 with
      | Some state1, Some state2 -> Cesk_types.print_difference state1 state2
      | _ -> raise (BadArguments
                     ("at least one of the state ids is incorrect " ^
                        "(use -v 2 and look in the produced graph to find them"))
    end
  | _ -> raise (BadArguments
                  ("two node ids should be specified (using -t1 and -t2; " ^
                     "use -v 2 and look into the produced graph to find them)"))

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
      | Params.MHP -> mhp
      | Params.SetConflicts -> detect_conflicts ~handle_cas:false
      | Params.Conflicts -> detect_conflicts ~handle_cas:true
      | Params.UnretriedCas -> detect_unretried_cas
      | Params.DeadlockDetection -> detect_deadlocks
      | Params.CompareStates -> compare_states in
    let node = Parser.parse (Lexer.lex !input) in
    action node
  with
  | e -> print_string (Exceptions.string_of_exception e)
