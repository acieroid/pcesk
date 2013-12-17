open Types
open Store
open Set_lattice

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

(* Compare states (for debugging) *)
let compare_states s1 s2 =
  if s1 = s2 then
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
