open Cesk_types
open Parallel_garbage_collection
open Pcesk
open Pcesk_types
open Pviz
open Types

let step_thread1_ctx pstate t =
  if ThreadMap.mem t pstate.threads then
    let contexts = ThreadMap.find t pstate.threads in
    match ContextSet.elements contexts with
    | [ctx] -> Some (step_context pstate t ctx, ctx)
    | _ ->
      (* We only support CPOR when there is at most one context associated
         with a thread id. This may be adapted later *)
      failwith ("More than one context for a tid (got " ^
                (string_of_int (ContextSet.cardinal contexts)) ^ ")")
  else
    None

let step_thread1 pstate t =
  match step_thread1_ctx pstate t with
  | None -> []
  | Some res -> fst res

let step_thread1' pstate t =
  List.fold_left
    (fun s pstate' ->
       PStateSet.add
         (if !Params.gc then gc pstate' else pstate')
         s)
      PStateSet.empty
      (step_thread1 pstate t)

let are_independent pstate t1 t2 =
  let step_thread_aux t pstate set =
    PStateSet.union (step_thread1' pstate t) set in
  PStateSet.compare
    (PStateSet.fold (step_thread_aux t2)
       (step_thread1' pstate t1) PStateSet.empty)
    (PStateSet.fold (step_thread_aux t1)
       (step_thread1' pstate t2) PStateSet.empty)
  = 0

module CVMap = ThreadMap

module TidSet = Set.Make(struct
    type t = tid
    let compare = Pervasives.compare
  end)

let string_of_extendable extendable =
  "{" ^ (String.concat ", " (List.map string_of_tid
                               (TidSet.elements extendable))) ^ "}"

let string_of_cv cv =
  "{" ^ (String.concat ", \n\n"
           (List.map (fun (tid, (g, pstates)) ->
                String.concat "\n"
                   (List.map (fun pstate ->
                        (string_of_pstate "    " pstate))
                          (PStateSet.elements pstates)))
               (CVMap.bindings cv))) ^ "}"
  

let rec calc_cv_aux cv extendable =
  if TidSet.is_empty extendable then
    (* No more CV can be extended, we computed all the maximal CVs *)
    cv
  else
    (* Pick any tid in extendable *)
    let tid = TidSet.min_elt extendable in
    print_string ("Computing CV for thread " ^ (string_of_tid tid));
    print_newline ();
(*    print_string "CV: "; print_newline ();
    print_string (string_of_cv cv); print_newline (); *)
    let cont, cv, extendable =
      (* For every state in last(CV[tid]) *)
      PStateSet.fold
        (fun pstate (cont, cv, extendable) ->
           print_string "Picked pstate in last(CV[tid])";
           print_newline ();
           print_string (string_of_pstate "    " pstate);
           print_newline ();
           if cont then
             (* for every new pstate computed by the transition *)
             let s' = step_thread1' pstate tid in
             PStateSet.fold
               (fun pstate' (cont, cv, extendable) ->
                  print_string "Picked pstate in new last(CV[tid])";
                  print_newline ();
                  print_string (string_of_pstate "    " pstate');
                  print_newline ();
                  if cont then
                    (* if the transition is not independent from a transition
                       in one of the other CVs (except in the `last` component),
                       we cannot extend this CV anymore *)
                    let indep =
                      CVMap.for_all
                        (fun tid' (g, last) ->
                           G.fold_edges_e
                             (fun (ps, (tid, ctx), ps') indep ->
                                print_string (string_of_context ctx);
                                print_newline ();
                                indep &&
                                (PStateSet.mem ps' last ||
                                 (compare_pstates pstate' ps != 0 ||
                                  are_independent pstate' tid tid')))
                             g
                             true)
                        cv
                    in
                    print_string ("indep: " ^ (string_of_bool indep));
                    print_newline ();
                    if not indep then
                      (false, cv, TidSet.remove tid extendable)
                    else
                      (* we also cannot extend every CV which has a last
                         transition dependent from this transition *)
                      let extendable =
                        CVMap.fold
                          (fun tid' (g, last) extendable ->
                             if tid = tid' then
                               extendable
                             else
                               let indep =
                                 PStateSet.for_all
                                   (fun ps ->
                                      compare_pstates pstate' ps != 0 ||
                                      are_independent pstate' tid tid')
                                   last in
                               if indep then
                                 extendable
                               else begin
                                 print_string ("Removing tid from extendable " ^ (string_of_tid tid'));
                                 print_newline ();
                                 TidSet.remove tid (TidSet.remove tid' extendable)
                               end)
                          cv
                          extendable in
                      (* if the new state is already present, this CV is
                         infinite and we can stop computing it *)
                      let extendable = 
                        let (g, last) = CVMap.find tid cv in
                        if G.mem_vertex g pstate' then begin
                          print_string ("Removing tid from extendable " ^ (string_of_tid tid));
                          print_newline ();
                          TidSet.remove tid extendable
                        end else
                          extendable in
                      (cont, cv, extendable)
                  else
                    (cont, cv, extendable))
                 s'
                 (true, cv, extendable)
           else
             (cont, cv, extendable))
        (snd (CVMap.find tid cv))
        (true, cv, extendable) in
    (* Finally, we add the next transition and state to the CV *)
    print_string "Adding next transitions";
    print_newline ();
    let cv, extendable =
      if cont then
        let (g, last) = CVMap.find tid cv in
        let (new_g, new_last) =
          PStateSet.fold
            (fun pstate (gr, l) ->
               print_string ("Adding next of " ^ (string_of_tid tid) ^ "\n");
               print_string (string_of_pstate "--> " pstate);
               print_newline ();
               print_newline ();
               match step_thread1_ctx pstate tid with
               | Some (pstates, ctx) ->
                 List.fold_left
                   (fun (graph, last) pstate' ->
                      print_string (string_of_pstate "--- " pstate');
                      print_newline ();
                      (G.add_edge_e (G.add_vertex graph pstate')
                         (G.E.create pstate (tid, ctx) pstate'),
                       PStateSet.add pstate' last))
                   (gr, l)
                   pstates
               | None ->
                 (* TODO: remove from extendable *)
                 (g, l))
            last
            (g, PStateSet.empty) in
        (CVMap.add tid (new_g, new_last) cv,
         if PStateSet.is_empty new_last then
           TidSet.remove tid extendable
         else
           extendable)
      else
        (cv, extendable) in
    calc_cv_aux cv extendable

let calc_cv pstate =
  let initial tid contexts =
    let pstates =
      match ContextSet.elements contexts with
      | [] -> []
      | [ctx] ->
        List.map (fun pstate -> pstate, ctx)
          (step_thread1 pstate tid)
      | _ -> failwith ("More than one context for tid " ^
                       (string_of_tid tid)) in
    List.fold_left
      (fun (graph, last) (pstate', context) ->
         (G.add_edge_e (G.add_vertex graph pstate)
            (G.E.create pstate (tid, context) pstate'),
          PStateSet.add pstate' last))
      (G.add_vertex G.empty pstate, PStateSet.empty)
      pstates in
  let cv =
    ThreadMap.fold
      (fun tid contexts -> CVMap.add tid (initial tid contexts))
      pstate.threads
      CVMap.empty in
  let extendable =
    List.fold_left
      (fun s (k, _) -> TidSet.add k s)
      TidSet.empty
      (ThreadMap.bindings pstate.threads) in
  print_string "CV:"; print_newline ();
  print_string (string_of_cv cv);
  print_newline ();
  print_string ("extendable:" ^ (string_of_extendable extendable));
  print_newline ();
  calc_cv_aux cv extendable

let eval e =
  let module Exploration = (val !Params.exploration) in
  let initial_state = inject e in
  let a_halt = initial_state.a_halt in
  let extract_finals pstate =
    let initial_thread_contexts =
      ContextSet.elements
        (ThreadMap.find InitialTid pstate.threads) in
    List.fold_left (fun acc c ->
        match c.cexp, c.caddr with
        | Value result, addr when addr = a_halt ->
          (result, c.cenv, pstate.pstore) :: acc
        | _ -> acc)
      [] initial_thread_contexts
  and todo = Exploration.create initial_state
  and interrupted () = match Unix.select [Unix.stdin] [] [] 0. with
    | (_ :: _, _, _) -> true
    | _ -> false in
  let rec loop visited finished graph i =
    if interrupted () || Exploration.is_empty todo then
      finished, graph
    else
      let pstate = Exploration.pick todo in
      let found = PStateSet.mem pstate visited in
      if found then begin
        print_string "state found"; print_newline ();
        loop visited finished graph (i+1)
      end
      else match extract_finals pstate with
        | [] ->
          if List.length (ThreadMap.bindings pstate.threads) == 1 then begin
            print_string "normal step"; print_newline ();
            (* Only one thread, same as without CPOR *)
            let pstates = List.map (fun (transition, pstate) ->
                if !Params.gc_after then
                  (transition, gc pstate)
                else
                  (transition, pstate))
                (step pstate) in
            let source = G.V.create pstate
            and dests = List.map (fun (_, pstate) -> G.V.create pstate) pstates in
            let edges = List.map (fun (transition, dest) -> G.E.create source
                                     transition dest) pstates in
            let graph' =
              List.fold_left G.add_edge_e
                (List.fold_left G.add_vertex graph dests) edges in
            if !Params.progress && i mod 1000 = 0 then begin
              print_string ("\r" ^ string_of_int (G.nb_vertex graph'));
              flush_all ()
            end;
            if !Params.verbose >= 1 then begin
              print_string (string_of_pstate "==> " pstate);
              print_newline ();
              List.iter (fun (_, pstate') ->
                  print_string (string_of_pstate "    " pstate');
                  print_newline ())
                pstates;
              print_newline ();
            end;
            Exploration.add todo (List.map snd pstates);
            loop (PStateSet.add pstate visited) finished graph' (i+1)
          end else begin
            print_string "CPOR "; print_int i; print_newline ();
            (* More than one thread, do the CPOR *)
            let cv = calc_cv pstate in
            let (graph', visited') = CVMap.fold
                (fun tid (g, last) (graph, visited) ->
                   output_graph g ("/tmp/graph-" ^ (string_of_int i) ^ "-" ^
                                   (string_of_tid tid) ^ ".dot");
                   let visited = (PStateSet.fold
                      (fun pstate visited ->
                         let pstates = step_thread1 pstate tid in
                         print_string (string_of_pstate "==> " pstate);
                         print_newline ();
                         List.iter (fun pstate' ->
                             print_string (string_of_pstate "    " pstate');
                             print_newline ())
                           pstates;
                         print_newline ();
                         Exploration.add todo pstates;
                         List.fold_left
                           (fun set pstate -> PStateSet.remove pstate set)
                           visited pstates)
                      last
                      (G.fold_vertex PStateSet.add g visited)) in
                   (GOper.union graph g,
                    visited))
                cv
                (graph, visited) in
            loop visited' finished graph' (i+1)
          end
        | res ->
          print_string "finished"; print_newline ();
          loop (PStateSet.add pstate visited) (res @ finished) graph (i+1)
  in
  let initial_graph = G.add_vertex G.empty (G.V.create initial_state) in
  loop PStateSet.empty [] initial_graph 0
