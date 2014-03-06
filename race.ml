open Cesk_types
open Env
open Pcesk_types
open Pviz

module TagPairSet = Set.Make (struct
    type t = Ast.tag * Ast.tag
    let compare (t1, t2) (t1', t2') =
      if (t1 = t1' && t2 = t2') || (t1 = t2' && t2 = t1') then
        0
      else
        Util.order_concat [Pervasives.compare t1 t1';
                           Pervasives.compare t2 t2']
  end)

(* A program may contain a race condition if there exists a state where
   two different contexts contains either a read and a write to a
   variable which points to the same address in both threads, or a write
   and a write on such a variable.
   This is very similar to a MHP analysis. In fact, it is a MHP analysis
   that checks if a read and a write (or a write and a write) happen in
   parallel on a variable that points to the same address
*)

let race graph =
  let cas_can_write = function
    | `Equal | `Unknown -> true
    | _ -> false in
  let written_to pstate ctx =
    match ctx.cexp with
    | Node (Ast.Set ((v, _), _), tag) ->
      Some (v, tag)
    | Node (Ast.Cas ((v, _), e_old, _), tag)
      when cas_can_write (Cesk.cas_case v e_old ctx.cenv pstate.pstore) -> 
      Some (v, tag)
    | _ -> None
  and read_from pstate ctx =
    match ctx.cexp with
    | Node (Ast.Identifier v, tag') ->
      Some (v, tag')
    | _ -> None in
  let find_writes pstate tid =
    (* Find all the set! currently being evaluated in pstate's threads
       with the given tid, and return them as a list of tid and addresses *)
    let contexts = ContextSet.elements (ThreadMap.find tid pstate.threads) in
    BatList.filter_map (fun ctx ->
        match written_to pstate ctx with
        | Some (v, tag) ->
          Some (tid, Env.lookup ctx.cenv v, tag)
        | None ->
          None)
      contexts
  and find_races pstate (t, a, tag) tid =
    (* Given that a thread with id t writes in address a in this pstate, 
       try to find another write or read to the same address, in thread
       tid, and returns a list of pairs of conflicting expressions *)
    let contexts = ContextSet.elements (ThreadMap.find tid pstate.threads) in
    BatList.filter_map (fun ctx ->
        match written_to pstate ctx, read_from pstate ctx with
        | (Some (v, tag'), None)
        | (None, Some (v, tag')) ->
          let addr = Env.lookup ctx.cenv v in
          if addr = a then
            (* Potential race, we just have to make sure it is not a
               race between an expression and itself in the same
               thread (which is not a race) *)
            if tid = t then
              (* A same thread id does not imply the same thread, so we 
                 count to see if there is only one access to this 
                 address in the threads with identifier tid *)
              let size = List.length (List.filter (fun ctx ->
                  match ctx.cexp with
                  | Node (Ast.Set ((v, _), _), _)
                  | Node (Ast.Identifier v, _)
                    when Env.lookup ctx.cenv v = addr -> true
                  | _ -> false)
                  contexts) in
              if size == 1 then
                None (* Only one access, no race *)
              else
                Some (tag, tag')
            else
              Some (tag, tag')
          else
            None
        | (None, None) ->
          None
        | _ ->
          failwith "Not implemented (simultaneous read and write from the same instruction)")
      contexts
  in
  let race_pstate pstate =
  (* Check if a given pstate may have a race condition. Return a list of
     the possible race condition as a list of pairs of tags *)
    let threads = pstate.threads in
    (* For every thread that will evaluate a set!, extract the address
       of the variable and see if there is another thread that reads or
       writes to this address *)
    let tids = List.map fst (ThreadMap.bindings threads) in
    let writes = List.concat (List.map (find_writes pstate) tids) in
    List.concat (List.map (fun w ->
        List.concat (List.map (find_races pstate w) tids))
        writes)
  in
  TagPairSet.elements (G.fold_vertex 
                     (fun pstate found ->
                        List.fold_left
                          (fun r s -> TagPairSet.add s r)
                          found (race_pstate pstate))
                     graph TagPairSet.empty)
