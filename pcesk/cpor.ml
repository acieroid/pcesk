open Cesk_types
open Parallel_garbage_collection
open Pcesk
open Pcesk_types
open Pviz
open Types

let step_context' pstate t c =
  List.fold_left
    (fun s pstate ->
       PStateSet.add
         (if !Params.gc then gc pstate else pstate)
         s)
    PStateSet.empty
    (step_context pstate t c)

let are_independent pstate t1 c1 t2 c2 =
  let step_context_aux t c pstate set =
    PStateSet.union (step_context' pstate t c) set in
  PStateSet.compare
    (PStateSet.fold (step_context_aux t2 c2)
       (step_context' pstate t1 c1) PStateSet.empty)
    (PStateSet.fold (step_context_aux t1 c1)
       (step_context' pstate t2 c2) PStateSet.empty)
  = 0

module CVMap = ThreadMap

module TidSet = Set.Make(struct
    type t = tid
    let compare = Pervasives.compare
  end)

let rec calc_cv_aux cv extendable =
  if TidSet.is_empty extendable then
    (* No more CV can be extended, we computed all the maximal CVs *)
    cv
  else
    (* Pick any tid in extendable *)
    let tid = TidSet.min_elt extendable in
    let cont, cv, extendable =
      (* For every state in last(CV[tid]) *)
      PStateMap.fold
        (fun pstate (_tid, context) (cont, cv, extendable) ->
           if cont then
             (* for every last (pstate, context) computed by the transition *)
             let s' = step_context' pstate tid context in
             PStateSet.fold
               (fun pstate' (cont, cv, extendable) ->
                  if cont then
                    (* if the transition is not independent from a transition
                       in one of the other CVs (except in the `last` component),
                       we cannot extend this CV anymore *)
                    let indep =
                      CVMap.for_all
                        (fun tid' (g, last) ->
                           G.fold_edges_e
                             (fun (ps, (_, ctx), ps') indep ->
                                indep &&
                                (PStateMap.mem ps' last ||
                                 (compare_pstates pstate' ps != 0 ||
                                  are_independent pstate' tid context tid' ctx)))
                             g
                             true)
                        cv
                    in
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
                                 PStateMap.for_all
                                   (fun ps (tid, ctx) ->
                                      compare_pstates pstate' ps = 0 &&
                                      are_independent pstate' tid context tid' ctx)
                                   last in
                               if indep then
                                 extendable
                               else
                                 TidSet.remove tid (TidSet.remove tid' extendable))
                          cv
                          extendable in
                      (* if the new state is already present, this CV is
                         infinite and we can stop computing it *)
                      let extendable = 
                        let (g, last) = CVMap.find tid cv in
                        if G.mem_vertex g pstate' then
                          TidSet.remove tid extendable
                        else
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
    let cv =
      if cont then
        CVMap.add tid
          (let (g, last) = CVMap.find tid cv in
           let (new_g, new_last) =
             PStateMap.fold
               (fun pstate (tid, ctx) (gr, l) ->
                  let pstates = step_context pstate tid ctx in
                  List.fold_left
                    (fun (graph, last) pstate' ->
                       (G.add_edge_e (G.add_vertex graph pstate')
                          (G.E.create pstate (tid, ctx) pstate'),
                        PStateMap.add pstate' (tid, ctx) last))
                    (g, last)
                    pstates)
               last
               (g, last) in
           (new_g, new_last))
          cv
      else
        cv in
    calc_cv_aux cv extendable

let calc_cv pstate =
  let initial tid contexts =
    let pstates =
      List.concat
        (List.map (fun context ->
            List.map (fun pstate' -> (pstate', context))
              (step_context pstate tid context))
            (ContextSet.elements contexts)) in
    List.fold_left
      (fun (graph, last) (pstate', context) ->
         (G.add_edge_e (G.add_vertex graph pstate)
            (G.E.create pstate (tid, context) pstate'),
          PStateMap.add pstate' (tid, context) last))
      (G.add_vertex G.empty pstate, PStateMap.empty)
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
        loop visited finished graph (i+1)
      end
      else match extract_finals pstate with
        | [] ->
          if List.length (ThreadMap.bindings pstate.threads) == 1 then begin
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
            (* More than one thread, do the CPOR *)
            let cv = calc_cv pstate in
            let (graph', visited') = CVMap.fold
                (fun tid (g, last) (graph, visited) ->
                   output_graph g ("/tmp/graph-" ^ (string_of_int i) ^ "-" ^
                                   (string_of_tid tid) ^ ".dot");
                   (PStateMap.iter
                      (fun pstate (tid, ctx) ->
                         let pstates = step_context pstate tid ctx in
                         print_string (string_of_pstate "==> " pstate);
                         print_newline ();
                         List.iter (fun pstate' ->
                             print_string (string_of_pstate "    " pstate');
                             print_newline ())
                           pstates;
                         print_newline ();
                         Exploration.add todo pstates)
                      last);
                   (GOper.union graph g,
                    G.fold_vertex PStateSet.add g visited))
                cv
                (graph, visited) in
            loop visited' finished graph' (i+1)
          end
        | res ->
          loop (PStateSet.add pstate visited) (res @ finished) graph (i+1)
  in
  let initial_graph = G.add_vertex G.empty (G.V.create initial_state) in
  loop PStateSet.empty [] initial_graph 0
