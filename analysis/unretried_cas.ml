open Cesk_types
open Deadlock
open Exploration
open Pcesk
open Pcesk_types
open Pviz
open Types
open Util

let has_false_successor =
  (* Check if a pstate has a successor for which a context associated to tid has
   * a #f control component *)
  has_successor (Value (AbsUnique (Boolean false)))

let find_cas graph =
  (* Find all the graph nodes that evaluate (cas v e1 e2) and lead to at least a
   * #f state *)
  G.fold_vertex
    (fun pstate acc ->
       let cas = extract_cas pstate in
       List.fold_left
         (fun acc (tid, tag) ->
            if has_false_successor graph pstate tid then
              (pstate, tid, tag) :: acc
            else
              acc)
         acc
         cas)
    graph
    []

let has_cas pstate tid tag =
  (* Check if the given pstate evaluates a cas of the given tag on the given
   * tid *)
  if ThreadMap.mem tid pstate.threads then
    let contexts = ThreadMap.find tid pstate.threads in
    ContextSet.exists (fun context ->
        match context.cexp with
        | Node (Ast.Cas _, tag') when tag' = tag -> true
        | _ -> false)
      contexts
  else
    false

let is_retried graph initial tid tag =
  (* A cas is retried if it has some successor that has the same cas in the
   * control component of a context associated with the same tid *)
  let todo = Dfs.create initial in
  let rec aux i visited =
    if Dfs.is_empty todo then
      (* Not found, the cas is not retried *)
      false
    else
      let pstate = Dfs.pick todo in
      if i > 0 && has_cas pstate tid tag then
        (* Correctly retried *)
        true
      else if PStateSet.mem pstate visited then
        (* Already visited, skip *)
        aux (succ i) visited
      else begin
        (* Visit the current state and continue *)
        Dfs.add todo (G.succ graph pstate);
        aux (succ i) (PStateSet.add pstate visited) end in
  aux 0 PStateSet.empty

let unretried_cas graph =
  (* A cas is not retried if it has a #f successor such that the same cas is
   * never retried.  Such a cas is a source of race conditions *)
  let unretried = BatList.filter_map
      (fun (pstate, tid, tag) ->
         if is_retried graph pstate tid tag then
           None
         else
           Some tag)
      (find_cas graph) in
  (* filter duplicates *)
  IntSet.elements
    (List.fold_left
       (fun s x -> IntSet.add x s)
       IntSet.empty
       unretried)
