open Types
open Cesk_types

(** Types and modules *)

module ThreadMap = Map.Make (Tid)

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

type thread_count = One | Infinity
module ThreadCountMap = Map.Make(Tid)

type threads = ContextSet.t ThreadMap.t
type pstate = {
  threads : threads;
  pstore : store;
  tcount : thread_count ThreadCountMap.t;
  a_halt : addr
}

let context_set_of_list l =
  let rec context_set_of_list' l acc = match l with
    | [] -> acc
    | hd :: tl -> context_set_of_list' tl (ContextSet.add hd acc) in
  context_set_of_list' l ContextSet.empty

(** String conversions *)

let string_of_context ?color:(color=true) context =
  let c s = if color then s else "" in
  match context.cexp with
  | Node n -> (c "\027[31m") ^ (Ast.string_of_node n) ^ (c "\027[0m")
  | Value v -> (c "\027[32m") ^ (string_of_value v) ^ (c "\027[0m")

let string_of_pstate ?color:(color=true) prefix pstate =
  prefix ^ "{" ^
    (String.concat ("\n" ^ prefix ^ " ")
       (List.map (fun (tid, cs) ->
            (Tid.string_of_tid tid) ^ ": " ^
              "{" ^ (String.concat ", " (List.map (string_of_context ~color)
                                         (ContextSet.elements cs))) ^
              "}")
          (ThreadMap.bindings pstate.threads))) ^
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
