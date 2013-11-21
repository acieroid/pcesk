open Util
open Types
open Cesk_types

(** Types and modules *)

module type TID = sig
  type t
  val initial : t
  val compare : t -> t -> int
  val next : t -> t
  val string_of_tid : t -> string
end

module ConcreteTID = struct
  type t = int
  let initial = 1
  let compare = Pervasives.compare
  let next t = t+1
  let string_of_tid = string_of_int
end

module ThreadMap = Map.Make (ConcreteTID)

type context = {
  cexp : exp;
  cenv : env;
  caddr : addr;
  cchange : kont_op;
  ctime : time;
}

module ContextSet = Set.Make (struct
    type t = context
    let compare = Pervasives.compare
end)

type threads = ContextSet.t ThreadMap.t
type pstate = threads * store

let context_set_of_list l =
  let rec context_set_of_list' l acc = match l with
    | [] -> acc
    | hd :: tl -> context_set_of_list' tl (ContextSet.add hd acc) in
  context_set_of_list' l ContextSet.empty

(** String conversions *)

let string_of_context c = match c.cexp with
  | Node n -> "\027[31m" ^ (Ast.string_of_node n) ^ "\027[0m"
  | Value v -> "\027[32m" ^ (string_of_value v) ^ "\027[0m"

let string_of_pstate prefix (threads, store) =
  prefix ^ "{" ^
    (String.concat ("\n" ^ prefix)
       (List.map (fun (tid, cs) ->
            (ConcreteTID.string_of_tid tid) ^ ": " ^
              "{" ^ (String.concat ", " (List.map string_of_context
                                         (ContextSet.elements cs))) ^
              "}")
          (ThreadMap.bindings threads))) ^
    "}"

(** Conversion between CESK state and PCESK state *)

let state_of_context c store =
  { exp = c.cexp;
    env = c.cenv;
    store = store;
    addr = c.caddr;
    change = c.cchange;
    time = c.ctime; }

let context_of_state state =
  { cexp = state.exp;
    cenv = state.env;
    caddr = state.addr;
    cchange = state.change;
    ctime = state.time }

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
