open Cesk_types
open Env
open Pcesk_types
open Pviz
open Types

module TagPairAddrSet = Set.Make (struct
    type t = Ast.tag * Ast.tag * addr
    let compare (t1, t2, a) (t1', t2', a') =
      if (t1 = t1' && t2 = t2') || (t1 = t2' && t2 = t1') then
        Pervasives.compare a a'
      else
        Util.order_concat [Pervasives.compare t1 t1';
                           Pervasives.compare t2 t2';
                           Pervasives.compare a a']
  end)

let cas_can_write = function
  (* Check if a cas can produce a write, given its result *)
  | `Equal | `Unknown -> true
  | _ -> false

let written_to ~handle_cas pstate ctx =
  (* Check if a write can be done in a context *)
  match ctx.cexp with
  | Node (Ast.Set ((v, _), _), tag) ->
    Some (v, tag)
  | Node (Ast.Cas ((v, _), e_old, _), tag)
    when handle_cas &&
         cas_can_write (Cesk.cas_case v e_old ctx.cenv pstate.pstore) ->
    Some (v, tag)
  | _ -> None

let read_from pstate ctx =
  (* Check if a read can be done in a context *)
  match ctx.cexp with
  | Node (Ast.Identifier v, tag') ->
    Some (v, tag')
  | _ -> None

let find_writes ~handle_cas pstate tid =
  (* Find all the set! currently being evaluated in pstate's threads with the
   * given tid, and return them as a list of tid and addresses *)
  let contexts = ContextSet.elements (ThreadMap.find tid pstate.threads) in
  BatList.filter_map (fun ctx ->
      match written_to ~handle_cas pstate ctx with
      | Some (v, tag) ->
        Some (tid, Env.lookup ctx.cenv v, tag)
      | None ->
        None)
    contexts

let find_conflicts ~handle_cas pstate (t, a, tag) tid =
  (* Given that a thread with id t writes in address a in this pstate, try to
   * find another write or read to the same address, in thread tid, and
   * returns a list of pairs of conflicting expressions *)
  let contexts = ContextSet.elements (ThreadMap.find tid pstate.threads) in
  BatList.filter_map (fun ctx ->
      match written_to ~handle_cas pstate ctx, read_from pstate ctx with
      | (Some (v, tag'), None)
      | (None, Some (v, tag')) ->
        let addr = Env.lookup ctx.cenv v in
        if addr = a then
          (* Potential conflict, we just have to make sure it is not a
           * conflict between an expression and itself in the same thread
           * (which is not a conflict) *)
          if tid = t then
            (* A same thread id does not imply the same thread, so we count to
             * see if there is only one access to this address in the threads
             * with identifier tid *)
            let size = List.length (List.filter (fun ctx ->
                match written_to ~handle_cas pstate ctx,
                      read_from pstate ctx with
                | (Some (v, _), None)
                | (None, Some (v, _))
                  when Env.lookup ctx.cenv v = addr -> true
                | (None, None) -> false
                | _ -> failwith ("Not implemented (simultaneous read " ^
                                 "and write from the same expression)"))
                contexts) in
            if size == 1 then
              None (* Only one access, no conflict *)
            else
              Some (tag, tag', a)
          else
            Some (tag, tag', a)
        else
          None
      | (None, None) ->
        None
      | _ ->
        failwith ("Not implemented (simultaneous read and write from the " ^
                  "same expression)"))
    contexts

let conflict_pstate ~handle_cas pstate =
  (* Check if a given pstate may have a conflict. Return a list of the
   * possible conflicts as a list of pairs of tags *)
  let threads = pstate.threads in
  (* For every thread that will evaluate a set!, extract the address of the
   * variable and see if there is another thread that reads or writes to this
   * address *)
  let tids = List.map fst (ThreadMap.bindings threads) in
  let writes = List.concat (List.map (find_writes ~handle_cas pstate) tids) in
  List.concat (List.map (fun w ->
      List.concat (List.map (find_conflicts ~handle_cas pstate w) tids))
      writes)

let is_cas node tag =
  match Ast.find_node tag node with
  | Some (Ast.Cas _, _) -> true
  | _ -> false

(* A program may contain a read/write or a write/write conflict if there exists
 * a state where two different contexts contains either a read and a write to a
 * variable which points to the same address in both threads, or a write and a
 * write on such a variable.  This is very similar to a MHP analysis.  In fact,
 * it is a MHP analysis that checks if a read and a write (or a write and a
 * write) happen in parallel on a variable that points to the same address *)
let conflicts
    ?handle_cas:(handle_cas=true)
    ?ignore_unique_cas:(ignore_unique_cas=false)
    graph node =
  let res = TagPairAddrSet.elements
      (G.fold_vertex
         (fun pstate found ->
            List.fold_left
              (fun r s -> TagPairAddrSet.add s r)
              found (conflict_pstate ~handle_cas pstate))
         graph TagPairAddrSet.empty) in
  if ignore_unique_cas then
    (* Drop the conflicts for which there is a rw and a ww conflict with the
     * same address, where the write part is the same cas, and if there is no
     * other conflict for this address *)
    let grouped = BatList.group
        (fun (_, _, a) (_, _, a') -> Pervasives.compare a a')
        res in
    List.concat
      (List.map
         (function
           | [(t1, t2, _)] as l ->
             if t1 = t2 && is_cas node t1 then
               (* Only a ww conflict between two cas on the same address, that
                  can be dropped *)
               []
             else
               l
           | [(t1, t2, _); (t1', t2', _)] as l ->
             let ncas = List.length (List.filter (is_cas node)
                                          [t1; t2; t1'; t2']) in
             if ncas = 3 then
               (* Got a rw & a ww, and they matches the pattern we want
                * (two cas writes on the same address one side, a read and a
                * cas write on the same address as previously on the other side,
                * we ignore it *)
               []
             else
               l
           | l -> l)
         grouped)
  else
    res
