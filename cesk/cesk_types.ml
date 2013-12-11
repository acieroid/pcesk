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
