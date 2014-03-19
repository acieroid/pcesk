open Types
open Cesk_types
open Pcesk_types
open Garbage_collection

(* Parallel version of the garbage collector. Reclaim addresses that are not
 * reachable from any of the state, and return the narrowed store *)
let gc pstate =
  let contexts = List.fold_left ContextSet.union ContextSet.empty
      (List.map snd (ThreadMap.bindings pstate.threads)) in
  let states = List.map (fun c -> state_of_context c pstate.pstore)
      (ContextSet.elements contexts) in
  let locations = union (List.map reachable states) in
  let store = Store.narrow pstate.pstore (AddressSet.elements locations) in
  { pstate with pstore = store }
