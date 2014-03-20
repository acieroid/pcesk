open Env
open Types
open Store
open Set_lattice

(** Types and modules *)

module Lattice = Set_lattice(struct let size = 10 end)
module Store = Store(Addr)(Lattice)
module Aval = Aval.AbstractAval
module Time = Time.AbstractTime

type exp =
  | Node of Ast.node (* eval state *)
  | Value of value   (* continuation state *)
type kont_op =
  | Push
  | Pop
  | Epsilon
type store = Store.t
type state = {
  exp : exp;
  env : env;
  store : store;
  addr : addr;
  change : kont_op;
  time : time;
}

(** State comparison *)
let compare_states s1 s2 =
  Util.order_concat [Pervasives.compare s1.exp s2.exp;
                     Env.compare s1.env s2.env;
                     Store.compare s1.store s2.store;
                     Addr.compare s1.addr s2.addr;
                     Pervasives.compare s1.change s2.change;
                     Time.compare s1.time s2.time]

let state_subsumes s1 s2 =
  (* See explanation in pcesk_types.ml *)
  let s1_without_store = { s2 with store = Store.empty }
  and s2_without_store = { s1 with store = Store.empty } in
  let cmp = compare_states s1_without_store s2_without_store
  in
  match cmp, Store.subsumes s1.store s2.store with
  | 0, true -> true
  | _ -> false

module StateOrdered = struct
  type t = state
  let compare = compare_states
end

(** Print differences between states *)
let print_difference s1 s2 =
  if s1 = s2 || compare_states s1 s2 = 0 then
    print_string "states are equal\n"
  else begin
    print_string "states are different:\n";
    if not (s1.exp = s2.exp) then
      print_string "  exps are different\n";
    if not (s1.env = s2.env) then
      print_string "  envs are different\n";
    if not (s1.store = s2.store) then
      print_string "  stores are different\n";
    if not (s1.addr = s2.addr) then
      print_string "  address are different\n";
    if not (s1.change = s2.change) then
      print_string "  changes are different\n";
    if not (s1.time = s2.time) then
      print_string "  times are different\n"
  end
