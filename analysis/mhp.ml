open Cesk_types
open Pcesk_types
open Pviz

(* Two expressions may happen in parallel if there exists a state of the state
 * graph where two different contexts contains each expression (see A Family
 * of Abstract Interpretation of Concurrent Higher-Order Programs, section 3.7).
 * mhp will return true when the expressions tagged as tag1 and tag2 may happen
 * in parallel, according to this definition. It traverse the given graph in
 * order to discover the states, containing the contexts.
*)

let mhp graph tag1 tag2 =
  (* Check if a set of context contains an expression with the given tag *)
  let contains_expression tag contexts =
    ContextSet.exists (fun context -> match context.cexp with
        | Node (_, tag') when tag = tag' -> true
        | _ -> false) contexts in
  (* Check if the two expressions can happen in parallel in the given pstate *)
  let mhp_pstate pstate =
    let threads = pstate.threads in
    if tag1 = tag2 then
      let ts = ThreadMap.filter (fun _ -> contains_expression tag1) threads in
      if List.length (ThreadMap.bindings ts) == 1 then
        (* Only one thread id has this tag, but maybe there are more than one
         * context under this thread id *)
        let _, contexts = ThreadMap.choose ts in
        ContextSet.cardinal contexts > 1
      else
        List.length (ThreadMap.bindings ts) > 1
    else
      ThreadMap.exists (fun _ -> contains_expression tag1) threads &&
      ThreadMap.exists (fun _ -> contains_expression tag2) threads in
  (* Go over all the graph to find one state where the two expressions may
   * happen in parallel. *)
  (* TODO: the computation could be stopped as soon as a satisfying state is
   * found *)
  G.fold_vertex (fun pstate found -> found || mhp_pstate pstate) graph false
