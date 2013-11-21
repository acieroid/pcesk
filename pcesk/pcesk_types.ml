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

type thread_count = One | Infinity
module ThreadCountMap = Map.Make(ConcreteTID)

type threads = ContextSet.t ThreadMap.t
type pstate = threads * store * thread_count ThreadCountMap.t

let context_set_of_list l =
  let rec context_set_of_list' l acc = match l with
    | [] -> acc
    | hd :: tl -> context_set_of_list' tl (ContextSet.add hd acc) in
  context_set_of_list' l ContextSet.empty

(** String conversions *)

let string_of_context c = match c.cexp with
  | Node n -> "\027[31m" ^ (Ast.string_of_node n) ^ "\027[0m"
  | Value v -> "\027[32m" ^ (string_of_value v) ^ "\027[0m"

let string_of_pstate prefix (threads, store, tcount) =
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
