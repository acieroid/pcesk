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

let has_cycle_to_itself graph initial =
  (* Check if a pstate has a cycle that points back to itself. *)
  let todo = Dfs.create (initial, [initial]) in
  let rec aux i visited =
    if Dfs.is_empty todo then
      (* Finished, no cycle *)
      false
    else
      let (pstate, history) = Dfs.pick todo in
      if i > 0 && compare_pstates initial pstate == 0 then begin
        (* Found a cycle *)
        if !Params.verbose >= 2 then begin
          print_string "History:\n";
          (List.iter
             (fun pstate ->
                print_string (string_of_pstate "" pstate);
                print_newline ())
             (List.rev history));
        end;
        true
      end else if PStateSet.mem pstate visited then
        (* State already visited, skip it *)
        aux (i+1) visited
      else begin
        (* New state, different from the initial one, add its successors and
         * continue visiting *)
        let succ = List.map (fun s -> (s, s::history)) (G.succ graph pstate) in
        Dfs.add todo succ;
        aux (i+1) (PStateSet.add pstate visited) end in
  aux 0 PStateSet.empty

module TidTagSet = Set.Make(struct
    type t = tid * tag
    let compare = Pervasives.compare
  end)

let deadlocks graph =
  (* A deadlock is present if we have a cycle from a pstate to itself, and if
   * that pstate evaluates a `cas` and never results in a successful evaluation
   * of the `cas` (ie. there is no #t state in its successors *)
  let deadlocks = BatList.filter_map
      (fun (pstate, tid, tag) ->
         if has_cycle_to_itself graph pstate then
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
