open Env
open Types
open Cesk_types

(** Types and modules *)

module ThreadMap = Map.Make (struct
  type t = tid
  let compare = Pervasives.compare
end)

type context = {
  cexp : exp;
  cenv : env;
  caddr : addr;
  cchange : kont_op;
  ctime : time;
}

let compare_contexts c1 c2 =
  Util.order_concat [Pervasives.compare c1.cexp c2.cexp;
                     Env.compare c1.cenv c2.cenv;
                     Addr.compare c1.caddr c2.caddr;
                     Pervasives.compare c1.cchange c2.cchange;
                     Time.compare c1.ctime c2.ctime]

module ContextSet = Set.Make (struct
    type t = context
    let compare = compare_contexts
 end)

type thread_count = One | Infinity
module ThreadCountMap = ThreadMap

type threads = ContextSet.t ThreadMap.t
type pstate = {
  threads : threads;
  nthreads : int; (* number of created threads (does not decrease) *)
  pstore : store;
  tcount : thread_count ThreadCountMap.t;
  a_halt : addr
}

let context_set_of_list l =
  let rec context_set_of_list' l acc = match l with
    | [] -> acc
    | hd :: tl -> context_set_of_list' tl (ContextSet.add hd acc) in
  context_set_of_list' l ContextSet.empty

(** State comparison *)
let compare_pstates_no_subsumption s1 s2 =
  Util.order_concat [ThreadMap.compare ContextSet.compare s1.threads s2.threads;
                     Pervasives.compare s1.nthreads s2.nthreads;
                     Store.compare s1.pstore s2.pstore;
                     ThreadMap.compare Pervasives.compare s1.tcount s2.tcount;
                     Pervasives.compare s1.a_halt s2.a_halt]


let compare_pstates s1 s2 =
      (* If two states are only different in their store, and the first state's
        store subsumes the second, then they are considered as equal (since all
        the behaviours found by exploring from the second state will be already
        found by exploring the first one). *)
      if !Params.subsumption then
        let s1_without_store = { s1 with pstore = Store.empty }
        and s2_without_store = { s2 with pstore = Store.empty } in
        let cmp = compare_pstates_no_subsumption s1_without_store s2_without_store
        in
        match cmp, Store.subsumes s1.pstore s2.pstore with
        | 0, true -> 0
        | 0, false -> Store.compare s1.pstore s2.pstore
        | n, _ -> n
      else
        compare_pstates_no_subsumption s1 s2

module PStateOrdered = struct
    type t = pstate
    let compare = compare_pstates
end

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
            (string_of_tid tid) ^ ": " ^
              "{" ^ (String.concat ", " (List.map (string_of_context ~color)
                                         (ContextSet.elements cs))) ^
              "}")
          (ThreadMap.bindings pstate.threads))) ^
    "}\n" ^
    prefix ^ (Store.string_of_store pstate.pstore)

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

(** Print differences between states *)
let print_difference s1 s2 =
  if s1 = s2 || compare_pstates s1 s2 = 0 then
    print_string "pstates are equal"
  else begin
    print_string "pstates are different:\n";
    if not (s1.threads = s2.threads) then
      print_string "  threads are different\n";
    if not (s1.nthreads = s2.nthreads) then
      print_string "  nthreads are different\n";
    if not (Store.compare s1.pstore s2.pstore = 0) then begin
      print_string "  pstores are different\n";
      if Store.subsumes s1.pstore s2.pstore then
        print_string "    s1.pstore subsumes s2.pstore\n";
    end;
    if not (s1.tcount = s2.tcount) then
      print_string "  tcounts are different\n";
    if not (s1.a_halt = s2.a_halt) then
      print_string "  a_halts are different\n"
  end
