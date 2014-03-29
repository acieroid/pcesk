open Cesk_types
open Pcesk_types
open Pviz

let is_potential_deadlock pstate =
  (* Check if a state is a potential deadlock, that is if it only contains
   * control expressions that are either joins or acquires. It is only a
   * potential deadlock because we don't inspect whether the acquires will
   * succeed here *)
  ThreadMap.for_all
    (fun _ contexts ->
       ContextSet.for_all (fun context ->
           match context.cexp with
           | Node (Ast.Acquire _, _)
           | Node (Ast.Join _, _) -> true
           | _ -> false)
         contexts)
    pstate.threads

let deadlocks graph =
  (* A deadlock is present if there is a state where every context is either
   * trying to acquire a lock, or waiting on another thread; and if this state
   * does not have any successor *)
  let pstates =
    (* Potential deadlocks (states with only acquires/joins) *)
    G.fold_vertex (fun pstate l ->
      if is_potential_deadlock pstate then
        pstate :: l
      else
        l) graph [] in
  (* Keep only the states that have a successor *)
  List.filter (fun pstate -> G.succ graph pstate = []) pstates
