open Types
open Exceptions
open Cesk_types
open Cesk_base
open Pcesk_types
open Pviz
open Parallel_garbage_collection

module Tid = Tid.ConcreteTID

(** Helper functions *)
let merge_threads context tcount tid x y = match x, y with
  | Some x, Some y ->
    Some (ContextSet.union
            (match ThreadCountMap.find tid tcount with
             | One -> if !Params.threads_strong_updates then
                 ContextSet.remove context x
               else
                 x
             | Infinity -> x)
            y)
  | Some x, None | None, Some x -> Some x
  | None, None -> None

let remove_thread context tid x y =
  let simplify v =
    if ContextSet.is_empty v then None else Some v in
  match x, y with
  | Some x, Some y ->
    simplify (ContextSet.union
                (ContextSet.remove context x)
                (ContextSet.remove context y))
  | Some x, None | None, Some x ->
    simplify (ContextSet.remove context x)
  | None, None -> None

let merge_tids tid x y = match x, y with
  | Some x, Some y -> Some Infinity
  | Some x, None | None, Some x -> Some x
  | None, None -> None

(** Stepping *)

let step_spawn pstate tid context tag e =
  let tid' = Tid.newtid pstate.nthreads context pstate.threads
  and context'' = {context with
                   cexp = Node e;
                   caddr = HaltAddr;
                   cchange = Epsilon;
                   (* TODO probably not the best initial time one could find
                    * (since the corresponding address will already be used by
                    * other threads in the store). It should depend on the tid
                    * I guess *)
                   ctime = Time.initial} in
  let context' = {context with
                  cexp = Value (AbsUnique (Tid tid'));
                  cchange = Epsilon;
                  ctime = Time.tick context.ctime e} in
  [{ pstate with
     threads = ThreadMap.merge (merge_threads context pstate.tcount)
         pstate.threads
         (ThreadMap.add tid (ContextSet.singleton context')
            (ThreadMap.singleton tid'
               (ContextSet.singleton context'')));
     tcount = ThreadMap.merge merge_tids
         pstate.tcount
         (ThreadMap.singleton tid' One);
     nthreads = pstate.nthreads + 1}]

let step_join pstate tid context tag e =
  let thread_addresses = eval_atomic e context.cenv pstate.pstore in
  List.concat
    (List.map (function
         | AbsUnique (Tid t) ->
           let values =
             try
               Lattice.conc (store_lookup pstate.pstore (TidAddr t))
             with
               UnboundAddress _ ->
               (* thread is still computing, we're blocked until it halts *)
               []
           in
           List.map (fun v ->
               let context' =
                 { context with
                   cexp = Value v;
                   ctime = Cesk.tick (state_of_context context pstate.pstore)
                 } in
               { pstate with
                 threads = ThreadMap.merge (merge_threads context pstate.tcount)
                     pstate.threads
                     (ThreadMap.singleton tid
                        (ContextSet.add context'
                           (if !Params.join_strong then
                              ContextSet.empty
                            else
                              ContextSet.singleton context))) })
             values
         | _ -> []) (Lattice.conc thread_addresses))

let step_parallel pstate tid context = function
  | Ast.Spawn e, tag -> step_spawn pstate tid context tag e
  | Ast.Join e, tag -> step_join pstate tid context tag e
  | _ -> failwith "Should not happen"

let step_halt pstate tid context value =
  [{ pstate with
     pstore = store_extend1 pstate.pstore (TidAddr tid) value;
     threads =
       if !Params.remove_threads then
         ThreadMap.merge (remove_thread context)
           pstate.threads
           ThreadMap.empty
       else
         pstate.threads;
     tcount =
       (* TODO: it is not precised in A Family of... if the count should be
        * decreased only when halted threads are removed, or in general. Here
        * we do it only when halted threads are removed *)
       if !Params.remove_threads then
         match ThreadMap.find tid pstate.tcount with
         (* If it was the only thread, remove its count (since it halted) *)
         | One -> ThreadMap.remove tid pstate.tcount
         (* Already more than one thread, don't do anything *)
         | Infinity -> pstate.tcount
       else
         pstate.tcount
   }]

let step_cesk pstate tid context =
  let state = state_of_context context pstate.pstore in
  let states' = Cesk.step state in
  List.map (fun state ->
      { pstate with
        threads = ThreadMap.merge (merge_threads context pstate.tcount)
            pstate.threads
            (ThreadMap.singleton tid
               (ContextSet.singleton (context_of_state state)));
        pstore = state.store})
    states'

let step_context pstate tid context =
  (* Step a context, creating a (or multiple) new pstate for each stepped
   * context *)
  match context.cexp with
  | Node ((Ast.Spawn _, _) as n)
  | Node ((Ast.Join _, _) as n) ->
    step_parallel pstate tid context n
  | Node _ ->
    step_cesk pstate tid context
  | Value v ->
    if context.caddr = HaltAddr then
      step_halt pstate tid context v
    else
      step_cesk pstate tid context

let step_contexts pstate (tid, cs) =
  List.concat (List.map
                 (fun context ->
                    List.map (fun ps -> ((tid, context), ps))
                      (step_context pstate tid context))
                 (ContextSet.elements cs))

let step pstate =
  let pstate = if !Params.gc then gc pstate else pstate in
  List.concat
    (List.map (step_contexts pstate) (ThreadMap.bindings pstate.threads))

(** Injection *)
let inject e =
  let tid = InitialTid in
  let state = Cesk.inject e in
  { threads = ThreadMap.singleton tid
        (ContextSet.singleton (context_of_state state));
    pstore = state.store;
    tcount = ThreadCountMap.singleton tid One;
    nthreads = 1;
  }

(** Evaluation *)
module PStateSet = Set.Make(PStateOrdered)

(* Extract the final values of a pstate, ie. the values returned by the main
  * thread when it halted (if it did) *)
let extract_finals pstate =
  if ThreadMap.mem InitialTid pstate.threads then
    let initial_thread_contexts =
      ContextSet.elements
        (ThreadMap.find InitialTid pstate.threads) in
    List.fold_left (fun acc c ->
        match c.cexp, c.caddr with
        | Value result, HaltAddr ->
          (result, c.cenv, pstate.pstore) :: acc
        | _ -> acc)
      [] initial_thread_contexts
  else
    (* We assume that the caller of this function will handle the fact that not
     * having a main thread means that the program should stop *)
    []

let eval e =
  let module Exploration = (val !Params.exploration) in
  let initial_state = inject e in
  let todo = Exploration.create initial_state
  (* Stop the execution if there are some input on stdin (allows to inspect
   * the current state space) *)
  and interrupted () = match Unix.select [Unix.stdin] [] [] 0. with
    | (_ :: _, _, _) -> true
    | _ -> false in
  let rec loop visited finished graph i =
    if interrupted () || Exploration.is_empty todo then
      finished, graph
    else
      let pstate = Exploration.pick todo in
      let found = PStateSet.mem pstate visited in
      if found then
        loop visited finished graph (i+1)
      else
        let subsumed_by =
          if !Params.subsumption then
            PStateSet.filter (fun pstate' -> pstate_subsumes pstate' pstate)
              visited
          else
            PStateSet.empty in
        if not (PStateSet.is_empty subsumed_by) then
          (* This state is subsumed by another, we remove it from the graph and
           * change all its incoming edges to point towards the subsuming state.
           * We then skip this state *)
          let subsuming_pstate =
            (* We pick the max, but there should only be one subsuming state *)
            PStateSet.max_elt subsumed_by in
          let preds = G.pred_e graph pstate in
          let graph = List.fold_left (fun graph (ps, e, _) ->
              G.add_edge_e graph (ps, e, subsuming_pstate))
              (G.remove_vertex graph pstate)
              preds in
          loop (PStateSet.add pstate visited) finished graph (i+1)
        else match extract_finals pstate with
        | [] ->
          let pstates = List.map (fun (transition, pstate) ->
              if !Params.gc_after then
                (transition, gc pstate)
              else
                (transition, pstate))
              (step pstate) in
          let source = G.V.create pstate
          and dests = List.map (fun (_, pstate) -> G.V.create pstate) pstates in
          let edges = List.map (fun (transition, dest) -> G.E.create source
                                   transition dest) pstates in
          let graph' =
            List.fold_left G.add_edge_e
              (List.fold_left G.add_vertex graph dests) edges in
          if !Params.progress && i mod 1000 = 0 then begin
            print_string ("\r" ^ string_of_int (G.nb_vertex graph'));
            flush_all ()
          end;
          if !Params.verbose >= 1 then begin
            print_string (string_of_pstate "==> " pstate);
            print_newline ();
            List.iter (fun (_, pstate') ->
                print_string (string_of_pstate "    " pstate');
                print_newline ())
              pstates;
            print_newline ();
          end;
          Exploration.add todo (List.map snd pstates);
          loop (PStateSet.add pstate visited) finished graph' (i+1)
        | res ->
          loop (PStateSet.add pstate visited) (res @ finished) graph (i+1)
  in
  let initial_graph = G.add_vertex G.empty (G.V.create initial_state) in
  loop PStateSet.empty [] initial_graph 0
