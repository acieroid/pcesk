open Types
open Cesk_types
open Pcesk_types
open Garbage_collection

(** Helper functions *)
let merge_threads context tcount tid x y = match x, y with
  | Some x, Some y ->
    Some (ContextSet.union
            (match ThreadCountMap.find tid tcount with
             | One -> ContextSet.remove context x
             | Infinity -> x)
            y)
  | Some x, None | None, Some x -> Some x
  | None, None -> None

let merge_tids tid x y = match x, y with
  | Some x, Some y -> Some Infinity
  | Some x, None | None, Some x -> Some x
  | None, None -> None

(** Stepping *)

let step_spawn pstate tid context tag e =
  let tid' = ConcreteTID.next tid (* TODO next isn't correct here *)
  and context'' = {context with
                   cexp = Node e;
                   caddr = pstate.a_halt;
                   cchange = Epsilon;
                   (* TODO probably not the best initial time one could find
                      (since the corresponding address will already be used by
                      other threads in the store). It should depend on the tid
                      I guess *)
                   ctime = Time.initial} in
  let context' = {context with
                  (* TODO: first-class tid *)
                  cexp = Value (AbsUnique (Integer tid'));
                  cchange = Epsilon;
                  ctime = Time.tick context.ctime e} in
  [{ pstate with
    threads = ThreadMap.merge (merge_threads context pstate.tcount)
        pstate.threads
        (ThreadMap.singleton tid'
           (ContextSet.singleton context''));
    tcount = ThreadMap.merge merge_tids
        pstate.tcount
        (ThreadMap.singleton tid' One) }]

let step_join pstate tid context tag e =
  raise Exceptions.NotYetImplemented

let step_cas pstate tid context tag name e1 e2 =
  raise Exceptions.NotYetImplemented

let step_parallel pstate tid context  = function
  | Ast.Spawn e, tag -> step_spawn pstate tid context tag e
  | Ast.Join e, tag -> step_join pstate tid context tag e
  | Ast.Cas ((name, _), e1, e2), tag ->
    step_cas pstate tid context tag name e1 e2
  | _ -> failwith "Should not happen"

let step pstate =
  let step_context tid context =
    (* Step each context, creating a (or multiple) new pstate for each stepped
       context *)
    let state = state_of_context context pstate.pstore in
    match state.exp with
    | Node ((Ast.Spawn _, _) as n)
    | Node ((Ast.Join _, _) as n)
    | Node ((Ast.Cas _, _) as n) ->
      let state = if !Params.gc then gc state else state in
      step_parallel pstate tid context n
    | _ ->
      let states' = Cesk.step state in
      List.map (fun state ->
          { pstate with
            threads = ThreadMap.merge (merge_threads context pstate.tcount)
                pstate.threads
                (ThreadMap.singleton tid
                   (ContextSet.singleton (context_of_state state)));
            pstore = state.store})
        states' in
  let step_contexts (tid, cs) =
    List.concat (List.map (step_context tid) (ContextSet.elements cs)) in
  List.concat (List.map step_contexts (ThreadMap.bindings pstate.threads))

(** Injection *)
let inject e =
  let tid = ConcreteTID.initial in
  let state, a_halt = Cesk.inject e in
  {threads = ThreadMap.singleton tid
       (ContextSet.singleton (context_of_state state));
   pstore = state.store;
   tcount = ThreadCountMap.singleton tid One;
   a_halt = a_halt}

(** Evaluation *)
module PStateSet = Set.Make(struct
    type t = pstate
    let compare = Pervasives.compare
  end)

let eval e =
  let initial_state = inject e in
  let a_halt = initial_state.a_halt in
  let extract_finals pstate =
    let initial_thread_contexts =
      ContextSet.elements
        (ThreadMap.find ConcreteTID.initial pstate.threads) in
    List.fold_left (fun acc c ->
        match c.cexp, c.caddr with
        | Value result, addr when addr = a_halt ->
          (result, c.cenv, pstate.pstore) :: acc
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
