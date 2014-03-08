open Pcesk
open Pcesk_types
open Pviz
open Types

let step_context' pstate t c =
  List.fold_left (fun s x -> PStateSet.add x s) PStateSet.empty
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
    let _, cv, extendable =
      (* For every state in last(CV[tid]) *)
      PStateMap.fold
        (fun pstate context (cont, cv, extendable) ->
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
                      (* Finally, we add the next transition and state to the CV *)
                      let cv = TODO  in
                      (cont, cv, extendable)
                  else
                    (cont, cv, extendable))
                 s'
                 (true, cv, extendable)
           else
             (cont, cv, extendable))
        (CVMap.find tid cv)
        (true, cv, extendable) in
    calc_cv_aux cv extendable

let calc_cv pstate =
  let initial =
    (G.add_vertex G.empty (G.V.create pstate), PStateMap.empty) in
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
