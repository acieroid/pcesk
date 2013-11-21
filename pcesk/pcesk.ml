open Cesk_types
open Pcesk_types

(** Stepping *)

let step (threads, store) =
  let step_context tid c =
    (* TODO: handle spawn, join and cas *)
    let state = state_of_context c store in
    let states' = Cesk.step state in
    List.map (fun state ->
        (ThreadMap.merge
           (fun tid x y -> match x, y with
              | Some x, Some y -> Some (ContextSet.union x y)
              | Some x, None | None, Some x -> Some x
              | None, None -> None)
           threads
           (ThreadMap.singleton tid
              (ContextSet.singleton (context_of_state state))),
         state.store))
      states' in
  let step_contexts (tid, cs) =
    List.concat (List.map (step_context tid) (ContextSet.elements cs)) in
  List.concat (List.map step_contexts (ThreadMap.bindings threads))

(** Injection *)
let inject e =
  let state, a_halt = Cesk.inject e in
  (((ThreadMap.singleton ConcreteTID.initial
       (ContextSet.singleton (context_of_state state))),
   state.store),
   a_halt)

(** Evaluation *)
module PStateSet = Set.Make(struct
    type t = pstate
    let compare = Pervasives.compare
  end)

let eval e =
  let (initial_state, a_halt) = inject e in
  let extract_finals (threads, store) =
    let initial_thread_contexts =
      ContextSet.elements
        (ThreadMap.find ConcreteTID.initial threads) in
    List.fold_left (fun acc c ->
        match c.cexp, c.caddr with
        | Value result, addr when addr = a_halt ->
          (result, c.cenv, store) :: acc
        | _ -> acc)
      [] initial_thread_contexts
  and todo = Exploration.create initial_state in
  let rec loop visited finished =
    if Exploration.is_empty todo then
      finished
    else
      let pstate = Exploration.pick todo in
      try
        let _ = PStateSet.find pstate visited in
        loop visited finished
      with
        Not_found ->
        begin match extract_finals pstate with
          | [] ->
            let pstates = step pstate in
            if !Params.verbose >= 1 then begin
              print_string (string_of_pstate "==> " pstate);
              print_newline ();
              List.iter (fun pstate' ->
                  print_string (string_of_pstate "    " pstate');
                  print_newline ())
                pstates;
              print_newline ();
            end;
            Exploration.add todo pstates;
            loop (PStateSet.add pstate visited) finished
          | res ->
            loop (PStateSet.add pstate visited) (res @ finished)
        end
  in
  loop PStateSet.empty [], Viz.G.empty
