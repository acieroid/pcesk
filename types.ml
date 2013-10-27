open Store
open Env
open Concrete_addr

(** Modules *)

module Addr = Concrete_addr
module Env = Env(Addr)
module Store = Store(Addr)

(** Types *)

type env = Env.t
type addr = Addr.t
type node = Scheme_ast.scheme_node
type lam = string list * node
type value =
  | String of string
  | Integer of int
  | Boolean of bool
  | Symbol of string
  | Closure of lam * env
  | Primitive of prim
  | Kont of kont
and storable = value * env
and kont =
  | OperatorKont of node list * env * addr
  | OperandsKont of value * node list * value list * env * addr
  | HaltKont
and prim = string * (value list -> value)
