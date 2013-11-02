open Types
open Store
open Set_lattice

module Lattice = Set_lattice(struct let size = 10 end)
module Store = Store(Addr)(Lattice)
module Exploration = Exploration.Bfs

type exp =
  | Node of node
  | Value of value
type kont_op =
  | Push
  | Pop
  | Epsilon
type store = Store.t
type time = int
type state = {
  exp : exp;
  env : env;
  store : store;
  addr : addr;
  change : kont_op;
  time : time;
}
