open Pcesk
open Pcesk_types
open Pviz
open Types

let are_independent pstate t1 c1 t2 c2 =
  let step_context' pstate t c =
    List.fold_left (fun s x -> PStateSet.add x s) PStateSet.empty
      (step_context pstate t c) in
  let step_context_aux t c pstate set =
    PStateSet.union (step_context' pstate t c) set in
  PStateSet.compare
    (PStateSet.fold (step_context_aux t2 c2)
       (step_context' pstate t1 c1) PStateSet.empty)
    (PStateSet.fold (step_context_aux t1 c1)
       (step_context' pstate t2 c2) PStateSet.empty)
  = 0

let step_tid pstate tid =
  step_context pstate tid TODO

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
    let _, cv, extendable =
      (* For every state in last(CV[tid]) *)
      List.fold_left
        (fun (cont, cv, extendable) s ->
           if not cont then
             (cont, cv, extendable)
           else
             (* step the state *)
             let s' = step_tid s tid in
             let (g, last) = CVMap.find tid cv in
             (* check if any state is dependent of any other state in
                another CV (except in the `last` states of a CV *)
             let indep =
               List.for_all
                 (fun pstate ->
                    G.fold_vertex
                      (fun pstate' indep ->
                         indep && (not (PStateSet.mem pstate' last) &&
                                   not (are_independent pstate pstate')))
                      g true) in
             if not indep then
               (* if this is the case, remove tid from extendable and
                  stop here for this CV *)
               (false, cv, TidSet.remove tid extendable)
             else
               (* else, check the dependency between every element of
                  s' and the last states of every other CVs *)
               TODO)
        (true, cv, extendable)
        (snd (CVMap.find tid cv)) in
    calc_cv_aux cv extendable

let calc_cv pstate =
  let initial =
    (G.add_vertex G.empty (G.V.create pstate), PStateSet.singleton pstate) in
  let cv =
    ThreadMap.fold
      (fun tid contexts -> CVMap.add tid initial)
      pstate.threads
      CVMap.empty in
  let extendable =
    List.fold_left
      (fun s (k, _) -> TidSet.add k s)
      TidSet.empty
      (ThreadMap.bindings pstate.threads) in
  TODO
