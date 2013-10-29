open Env
open Address

(** Modules *)

module Addr = Mono_addr
module Env = Env(Addr)

(** Types *)

type env = Env.t
type addr = Addr.t
type node = Scheme_ast.scheme_node
type lam = (string * int) list * node
type value =
  | String of string
  | Integer of int
  | Boolean of bool
  | Symbol of string
  | Closure of lam * env
  | Primitive of prim
  | Kont of kont
(* TODO: In Abstracting Abstract Machines, Storable = Val x Env, here we
   only keep the value, since it is just simpler not to handle the
   environment. Is it still correct? The only CESK rule that uses the
   stored env is when looking at the value of an identifier, and it does
   not seem necessary to use this env there *)
and storable =
  | StorableValue of value
  | StorableKont of kont
and kont =
  | OperatorKont of node list * env * addr
  | OperandsKont of value * node list * value list * env * addr
  | HaltKont
and prim = string * (value list -> value)


(** String conversion *)

let string_of_kont = function
  | OperatorKont (_, _, a) -> "Operator(" ^ (Addr.string_of_address a) ^ ")"
  | OperandsKont (_, _, _, _, a) -> "Operands(" ^ (Addr.string_of_address a) ^ ")"
  | HaltKont -> "Halt"

let string_of_value = function
  | String s -> "\"" ^ s ^ "\""
  | Integer n -> string_of_int n
  | Boolean true -> "#t"
  | Boolean false -> "#f"
  | Symbol sym -> "'" ^ sym
  | Closure _ -> "#<closure>"
  | Primitive (name, _) -> "#<primitive " ^ name ^ ">"
  | Kont k -> "#<continuation " ^ (string_of_kont k) ^ ">"

(** Some operations on values *)
exception TypeError

let value_op_int f x y = match x, y with
  | Integer a, Integer b -> Integer (f a b)
  | _ -> raise TypeError

let value_comp_int f x y = match x, y with
  | Integer a, Integer b -> Boolean (f a b)
  | _ -> raise TypeError

let value_add = value_op_int (+)
let value_sub = value_op_int (-)
let value_mul = value_op_int ( * )
let value_div = value_op_int (/)

let value_neg x = match x with
  | Integer a -> Integer (-a)
  | _ -> raise TypeError

let value_gt = value_comp_int (>)
let value_lt = value_comp_int (<)
let value_eq x y = Boolean (x = y)
let value_neq x y = Boolean (x <> y)
