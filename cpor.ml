open Pcesk

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
