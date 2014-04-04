open Types
open Params
open Exceptions

let now () = Unix.gettimeofday ()

let print_infos time vertex edges =
  Printf.printf "%d/%d/%.3f\n" vertex edges time

let peval node =
  if !Params.cpor then Cpor.eval node else Pcesk.eval node

let eval node =
  if !Params.parallel then begin
    let res, graph = peval node in
    BatOption.may (Pviz.output_graph graph) !graph_file;
    close_in !input_chan;
    (res, Pviz.G.nb_vertex graph, Pviz.G.nb_edges graph)
  end else begin
    let res, graph = Cesk.eval node in
    BatOption.may (Viz.output_graph graph) !graph_file;
    close_in !input_chan;
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

let build_pgraph node =
  if !Params.parallel then
    let _, graph = peval node in
    graph
  else
    raise (BadArguments
             "cannot do this analysis without parallelism turned on (use -p)")

let mhp_g graph node t1 t2 exp1 exp2 =
  let may = Mhp.mhp graph t1 t2 in
  print_string ("The expressions " ^
                (Ast.string_of_node ~tags:true exp1) ^ " and " ^
                (Ast.string_of_node ~tags:true exp2) ^ " may " ^
                (if may then "" else "not ") ^ "happen in parallel\n")


let mhp node = match !Params.tag1, !Params.tag2 with
  | Some t1, Some t2 ->
    let e1, e2 = Ast.find_node t1 node, Ast.find_node t2 node in
    begin match e1, e2 with
      | Some exp1, Some exp2 ->
        mhp_g (build_pgraph node) node t1 t2 exp1 exp2
      | _ -> raise (BadArguments
                      ("at least one of the tags is incorrect " ^
                       "(use -target ast to find them)"))
    end
  | _ -> raise (BadArguments
                  "two tags should be specified (use -target ast to find them)")


let detect_conflicts_g graph handle_cas ignore_unique_cas node =
  let conflicts = Conflict.conflicts ~handle_cas ~ignore_unique_cas
      graph node in
  match conflicts with
  | [] -> print_string "No conflicts detected\n"
  | [(t1, t2, a)] ->
    begin match Ast.find_node t1 node, Ast.find_node t2 node with
      | Some e1, Some e2 ->
        print_string ("One conflict detected between the following " ^
                      "expressions:\n" ^
                      (Ast.string_of_node ~tags:true e1) ^ ", " ^
                      (Ast.string_of_node ~tags:true e2) ^ "\n")
      | _ ->
        print_string ("Conflict detected between unknown nodes! " ^
                      "(should not happen)")
    end
  | l ->
    print_string (string_of_int (List.length l) ^
                  " conflicts detected between the following pairs of" ^
                  " expressions:\n");
    List.iter (fun (t1, t2, a) ->
        match Ast.find_node t1 node, Ast.find_node t2 node with
        | Some e1, Some e2 ->
          print_string ((Ast.string_of_node ~tags:true e1) ^ ", " ^
                        (Ast.string_of_node ~tags:true e2) ^ "\n")
        | _ ->
          print_string ("Conflict detected between unknown nodes! " ^
                        "(should not happen)"))
      l

let detect_conflicts handle_cas ignore_unique_cas node =
  detect_conflicts_g (build_pgraph node) handle_cas ignore_unique_cas node

let detect_deadlocks_g graph skip_single node =
  let deadlocks = Deadlock.deadlocks graph skip_single in
  match deadlocks with
  | [] -> print_string "No deadlocks detected\n"
  | l ->
    print_string ((string_of_int (List.length l)) ^
                  " possible deadlocks detected, starting at the following" ^
                  " expressions:\n");
    List.iter (fun (tid, tag) ->
        match Ast.find_node tag node with
        | Some exp ->
          print_string (Ast.string_of_node ~tags:true exp);
          print_string (" [on tid " ^ (string_of_tid tid) ^ "]\n");
        | None ->
          print_string "Unknown node (should not happen)\n")
      l

let detect_deadlocks skip_single node =
  detect_deadlocks_g (build_pgraph node) skip_single node

let detect_ldeadlocks_g graph node =
  let deadlocks = Ldeadlock.deadlocks graph in
  match deadlocks with
  | [] -> print_string "No deadlocks detected\n"
  | l ->
    print_string ((string_of_int (List.length l)) ^
                  " possible deadlocks detected, at the following states:\n");
    List.iter (fun pstate ->
        print_string (Pcesk_types.string_of_pstate "    " pstate);
        print_newline ())
      l

let detect_ldeadlocks node =
  detect_ldeadlocks_g (build_pgraph node) node

let detect_unretried_cas_g graph node =
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

let detect_unretried_cas node =
  detect_unretried_cas_g (build_pgraph node) node

let detect_races node =
  let graph = build_pgraph node in
  detect_conflicts_g graph true true node;
  detect_unretried_cas_g graph node

let compare_states node = match !Params.tag1, !Params.tag2 with
  | Some t1, Some t2 ->
    begin if !Params.parallel then
        let _, graph = peval node in
        let s1, s2 = Pviz.find_node graph t1, Pviz.find_node graph t2 in
        match s1, s2 with
        | Some state1, Some state2 -> Pcesk_types.print_difference state1 state2
        | _ -> raise (BadArguments ("at least one of the state ids is " ^
                                    "incorrect (use -v 2 and look in the " ^
                                    "produced graph to find them"))
      else
        let _, graph = Cesk.eval node in
        let s1, s2 = Viz.find_node graph t1, Viz.find_node graph t2 in
        match s1, s2 with
        | Some state1, Some state2 -> Cesk_types.print_difference state1 state2
        | _ -> raise (BadArguments ("at least one of the state ids is " ^
                                    "incorrect (use -v 2 and look in the " ^
                                    "produced graph to find them"))
    end
  | _ -> raise (BadArguments
                  ("two node ids should be specified (using -t1 and -t2; " ^
                   "use -v 2 and look into the produced graph to find them)"))

let () =
  Arg.parse speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
  try
    if !Params.verbose >= 1 then begin
      print_string ("Running with: \n" ^ (string_of_configuration ()));
      print_newline ()
    end;
    let action = match !Params.target with
      | Params.Run -> run
      | Params.PrintAST -> print_ast
      | Params.MHP -> mhp
      | Params.SetConflicts -> detect_conflicts false false
      | Params.AllConflicts -> detect_conflicts true false
      | Params.Conflicts -> detect_conflicts true true
      | Params.UnretriedCas -> detect_unretried_cas
      | Params.Race -> detect_races
      | Params.DeadlockDetection -> detect_deadlocks true
      | Params.DeadlockDetectionSingle -> detect_deadlocks false
      | Params.LDeadlockDetection -> detect_ldeadlocks
      | Params.CompareStates -> compare_states in
    let node = Parser.parse (Lexer.lex !input_chan) in
    action node
  with
  | e -> print_string (Exceptions.string_of_exception e)
