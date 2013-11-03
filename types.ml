open Env
open Address

(** Modules *)

module Addr = Mono_addr
module Env = Env(Addr)

(** Types *)

type env = Env.t
type addr = Addr.t
type node = Scheme_ast.scheme_node
type lam = (string * int) list * (node list)
type prim_value =
  | String of string
  | Integer of int
  | Boolean of bool
  | Symbol of string
  | Cons of prim_value * prim_value
  | Nil
  | Unspecified
  | Closure of lam * env
  | Primitive of prim
  | Kont of kont
and value =
  | AbsUnique of prim_value
  | AbsString
  | AbsInteger
  | AbsBoolean
  | AbsSymbol
  | AbsList
and kont =
  | OperatorKont of int * node list * env * addr
  | OperandsKont of int * value * node list * value list * env * addr
  | BeginKont of int * node list * env * addr
  | DefineKont of int * string * env * addr
  | IfKont of int * node * node * env * addr
  | SetKont of int * string * env * addr
  | HaltKont
and prim = string * (value list -> value option)

(** String conversion *)

let string_of_kont = function
  | OperatorKont (t, _, _, _) -> "Operator-" ^ (string_of_int t)
  | OperandsKont (t, _, _, _, _, _) -> "Operands-" ^ (string_of_int t)
  | BeginKont (t, _, _, _) -> "Begin-" ^ (string_of_int t)
  | DefineKont (t, _, _, _) -> "Define-" ^ (string_of_int t)
  | IfKont (t, _, _, _, _) -> "If-" ^ (string_of_int t)
  | SetKont (t, _, _, _) -> "Set-" ^ (string_of_int t)
  | HaltKont -> "Halt"

let rec string_of_prim_value = function
  | String s -> "\"" ^ s ^ "\""
  | Integer n -> string_of_int n
  | Boolean true -> "#t"
  | Boolean false -> "#f"
  | Symbol sym -> "'" ^ sym
  | Cons (car, cdr) ->
    "(" ^ (string_of_prim_value car) ^ " . " ^
      (string_of_prim_value cdr) ^")"
  | Nil -> "()"
  | Unspecified -> "#<unspecified>"
  | Closure _ -> "#<closure>"
  | Primitive (name, _) -> "#<primitive " ^ name ^ ">"
  | Kont k -> "#<continuation " ^ (string_of_kont k) ^ ">"

let string_of_value = function
  | AbsUnique v -> string_of_prim_value v
  | AbsString -> "Str"
  | AbsInteger -> "Int"
  | AbsBoolean -> "Bool"
  | AbsSymbol -> "Sym"
  | AbsList -> "List"

(** Some operations on abstract values *)
let merge x y = match x, y with
  | AbsUnique (Integer n1), AbsUnique (Integer n2) ->
    Some (if n1 = n2 then AbsUnique (Integer n1) else AbsInteger)
  | AbsUnique (String s1), AbsUnique (String s2) ->
    Some (if s1 = s2 then AbsUnique (String s1) else AbsString)
  | AbsUnique (Symbol s1), AbsUnique (Symbol s2) ->
    Some (if s1 = s2 then AbsUnique (Symbol s1) else AbsSymbol)
  | AbsUnique (Boolean b1), AbsUnique (Boolean b2) ->
    Some (if b1 = b2 then AbsUnique (Boolean b1) else AbsBoolean)
  | AbsUnique (Cons (_, _) as l1), AbsUnique (Cons (_, _) as l2) ->
    Some (if l1 = l2 then AbsUnique l1 else AbsList)
  | AbsUnique Nil, AbsUnique Nil -> Some (AbsUnique Nil)
  | AbsUnique (Cons (_, _)), AbsUnique Nil
  | AbsUnique Nil, AbsUnique (Cons (_, _)) -> Some AbsList
  | AbsString, AbsString
  | AbsString, AbsUnique (String _)
  | AbsUnique (String _), AbsString -> Some AbsString
  | AbsInteger, AbsInteger
  | AbsInteger, AbsUnique (Integer _)
  | AbsUnique (Integer _), AbsInteger -> Some AbsInteger
  | AbsBoolean, AbsBoolean
  | AbsBoolean, AbsUnique (Boolean _)
  | AbsUnique (Boolean _), AbsBoolean -> Some AbsBoolean
  | AbsSymbol, AbsSymbol
  | AbsSymbol, AbsUnique (Symbol _)
  | AbsUnique (Symbol _), AbsSymbol -> Some AbsSymbol
  | AbsList, AbsList
  | AbsList, AbsUnique (Cons _)
  | AbsList, AbsUnique Nil
  | AbsUnique (Cons _), AbsList
  | AbsUnique Nil, AbsList -> Some AbsList
  | _ -> None

let value_subsumes x y = match x, y with
  | AbsUnique a, AbsUnique b -> a = b
  | AbsString, AbsString
  | AbsInteger, AbsInteger
  | AbsBoolean, AbsBoolean
  | AbsSymbol, AbsSymbol
  | AbsList, AbsList
  | AbsString, AbsUnique (String _)
  | AbsInteger, AbsUnique (Integer _)
  | AbsBoolean, AbsUnique (Boolean _)
  | AbsSymbol, AbsUnique (Symbol _) -> true
  | _ -> false

let value_op_int f x y = match x, y with
  | AbsInteger, _ | _, AbsInteger -> Some AbsInteger
  | AbsUnique (Integer v1), AbsUnique (Integer v2) ->
    Some (AbsUnique (Integer (f v1 v2)))
  | _ -> None

let value_comp_int f x y = match x, y with
  | AbsInteger, _ | _, AbsInteger -> Some AbsBoolean
  | AbsUnique (Integer v1), AbsUnique (Integer v2) ->
    Some (AbsUnique (Boolean (f v1 v2)))
  | _ -> None

let value_add = value_op_int (+)
let value_sub = value_op_int (-)
let value_mul = value_op_int ( * )
let value_div = value_op_int (/)

let value_neg x = match x with
  | AbsUnique (Integer a) -> Some (AbsUnique (Integer (-a)))
  | AbsInteger -> Some AbsInteger
  | _ -> None

let value_gt = value_comp_int (>)
let value_gte = value_comp_int (>=)
let value_lt = value_comp_int (<)
let value_lte = value_comp_int (<=)
let value_int_eq = value_comp_int (=)
let value_int_neq = value_comp_int (<>)
