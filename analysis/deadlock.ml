open Cesk_types
open Exploration
open Pcesk
open Pcesk_types
open Pviz
open Types

let extract_cas pstate =
  (* Extract the tid and tag of all cas in the pstate *)
  List.fold_left
    (fun acc (tid, contexts) ->
       let cas = ContextSet.fold
           (fun context acc ->
              match context.cexp with
              | Node (Ast.Cas _, tag) -> (tid, tag)::acc
              | _ -> acc)
           contexts
           [] in
       cas @ acc)
    []
    (ThreadMap.bindings pstate.threads)

let has_successor control graph pstate tid =
  (* Check if a pstate has a successor that has the given control component, for
   * a context associated with tid *)
  List.exists
    (fun pstate' ->
       ContextSet.exists
         (fun context -> context.cexp = control)
         (ThreadMap.find tid pstate'.threads))
    (G.succ graph pstate)

let has_true_successor =
  (* Check if a pstate has a successor for which a context associated to tid
   * evaluated to #t *)
  has_successor (Value (AbsUnique (Boolean true)))

let find_cas_only_false graph =
  (* Find all the graph nodes that evaluate (cas v e1 e2) and lead to only a #f
   * state (ie. the cas failed). *)
  G.fold_vertex
    (fun pstate acc ->
       let cas = extract_cas pstate in
       List.fold_left
         (fun acc (tid, tag) ->
            if not (has_true_successor graph pstate tid) then
              (pstate, tid, tag) :: acc
            else
              acc)
         acc
         cas)
    graph
    []

let has_cycle_to_itself graph skip_single initial =
  (* Check if a pstate has a cycle that points back to itself.  When skip_single
   * is true, skip the deadlocks that involve a single thread (eg. if a thread
   * acquire a lock and another thread want to acquire it, if the other thread
   * never gets executed the second thread will deadlock, even though in real
   * situation the first thread will at some point get executed *)
  let todo = Dfs.create (initial, [(IntTid (-1), initial)]) in
  let rec aux i visited =
    if Dfs.is_empty todo then
      (* Finished, no cycle *)
      false
    else
      let (pstate, history) = Dfs.pick todo in
      if i > 0 && compare_pstates initial pstate == 0 then
        if skip_single then
          (* Found a cycle *)
          let only_one_thread = snd
              (* Check if the cycle involves more than one thread *)
              (List.fold_left (fun (tid', res) (tid, _) ->
                   match tid', tid with
                   | IntTid (-1), _ -> (tid, true)
                   | _, IntTid (-1) -> (tid', true)
                   | _, _ when tid' = tid -> (tid, res)
                   | _, _ -> (tid, false))
                  (IntTid (-1), true)
                  history) in
          if only_one_thread then begin
            (* Not an interesting cycle, continue *)
            let succ = List.map (fun (_, (tid, _), s) -> (s, (tid, s)::history))
                (G.succ_e graph pstate) in
            Dfs.add todo succ;
            aux (i+1) (PStateSet.add pstate visited)
          end else
            (* Deadlock detected *)
            true
        else
          true
      else if PStateSet.mem pstate visited then
        (* State already visited, skip it *)
        aux (i+1) visited
      else begin
        (* New state, different from the initial one, add its successors and
         * continue visiting *)
        let succ = List.map (fun (_, (tid, _), s) -> (s, (tid, s)::history))
            (G.succ_e graph pstate) in
        Dfs.add todo succ;
        aux (i+1) (PStateSet.add pstate visited)
      end in
  aux 0 PStateSet.empty

module TidTagSet = Set.Make(struct
    type t = tid * tag
    let compare = Pervasives.compare
  end)

let deadlocks graph skip_single =
  (* A deadlock is present if we have a cycle from a pstate to itself, and if
   * that pstate evaluates a `cas` and never results in a successful evaluation
   * of the `cas` (ie. there is no #t state in its successors *)
  let deadlocks = BatList.filter_map
      (fun (pstate, tid, tag) ->
         if has_cycle_to_itself graph skip_single pstate then
           Some (tid, tag)
         else
           None)
      (find_cas_only_false graph) in
  (* Filter duplicates *)
  TidTagSet.elements
    (List.fold_left
       (fun s x -> TidTagSet.add x s)
       TidTagSet.empty
       deadlocks)
