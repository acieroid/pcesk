open Cesk_types
open Exploration
open Pcesk
open Pcesk_types
open Pviz
open Types

let extract_cas_tids pstate =
  (* Extract all the tids that are currently evaluating a cas in the
     given pstate *)
  List.fold_left
    (fun acc (tid, contexts) ->
       let has_cas = ContextSet.exists
           (fun context ->
              match context.cexp with
              | Node (Ast.Cas _, _) -> true
              | _ -> false)
           contexts in
       if has_cas then
         tid :: acc
       else
         acc)
    []
    (ThreadMap.bindings pstate.threads)

let has_successor control graph pstate tid =
  (* Check if a pstate has a successor that has the given control component, for
     a context associated with tid *)
  List.exists
    (fun pstate' ->
       ContextSet.exists
         (fun context -> context.cexp = control)
         (ThreadMap.find tid pstate'.threads))
    (G.succ graph pstate)

let has_true_successor =
  (* Check if a pstate has a successor for which a context associated to
     tid evaluated to #t *)
  has_successor (Value (AbsUnique (Boolean true)))

let find_cas_only_false graph =
  (* Find all the graph nodes that evaluate (cas v e1 e2) and lead to
       only a #f state (ie. the cas failed). *)
  G.fold_vertex
    (fun pstate acc ->
       let tids = extract_cas_tids pstate in
       List.fold_left
         (fun acc tid ->
            if not (has_true_successor graph pstate tid) then
              (pstate, tid) :: acc
            else
              acc)
         acc
         tids)
    graph
    []

let has_cycle_to_itself graph initial =
  (* Check if a pstate has a cycle that points back to itself.
     Implementation inspired from ocamlgraph's Travers.has_cycle. *)
  let todo = Dfs.create initial in
  let rec aux i visited =
    if Dfs.is_empty todo then
      (* Finished, no cycle *)
      false
    else
      let pstate = Dfs.pick todo in
      if i > 0 && compare_pstates initial pstate == 0 then
        (* Found a cycle *)
        (* TODO: we could extract the cycle to produce a trace that leads
           to a deadlock *)
        true
      else if PStateSet.mem pstate visited then
        (* State already visited, skip it *)
        aux (succ i) visited
      else begin
        (* New state, different from the initial one, add its successors
           and continue visiting *)
        Dfs.add todo (G.succ graph pstate);
        aux (succ i) (PStateSet.add pstate visited) end in
  aux 0 PStateSet.empty

let deadlocks graph =
  (* A deadlock is present if we have a cycle from a pstate to itself,
     and if that pstate evaluates a `cas` and never results in a
     successful evaluation of the `cas` (ie. there is no #t state in its
     successors *)
  List.filter
    (fun (pstate, tid) ->
       has_cycle_to_itself graph pstate)
    (find_cas_only_false graph)
