open Env
open Time
open Tid

(** Types *)

(* To use concrete values, change this to ConcreteTime and change aval
   to ConcreteAval.aval (later in this file) *)
module Time = AbstractTime
module Tid = ConcreteTID

type lam = (string * int) list * (Ast.node list)
type prim_value =
  | String of string
  | Integer of int
  | Boolean of bool
  | Symbol of string
  | Tid of Tid.t
  | Cons of prim_value * prim_value
  | Nil
  | Unspecified
  | Closure of lam * env
  | Primitive of string
  | Kont of kont
and value =
  | AbsUnique of prim_value
  | AbsString
  | AbsInteger
  | AbsBoolean
  | AbsSymbol
  | AbsList
and kont =
  | OperatorKont of int * Ast.node list * env * addr
  | OperandsKont of int * value * Ast.node list * value list * env * addr
  | BeginKont of int * Ast.node list * env * addr
  | LetRecKont of int * addr * (Ast.var * Ast.node) list *
                    Ast.node list * env * addr
  | IfKont of int * Ast.node * Ast.node * env * addr
  | SetKont of int * string * env * addr
  | CallccKont of int * env * addr
  | HaltKont
and prim = string * (value list -> value option)
and time = Time.t
and addr =
  | TagAddr of int * time
  | VarAddr of string * time
  | PrimAddr of string * time
  | KontAddr of Ast.node * time
and env = addr Env.t
type tag = int

(** String conversion *)

let string_of_kont = function
  | OperatorKont (t, _, _, _) -> "Operator-" ^ (string_of_int t)
  | OperandsKont (t, _, _, _, _, _) -> "Operands-" ^ (string_of_int t)
  | BeginKont (t, _, _, _) -> "Begin-" ^ (string_of_int t)
  | LetRecKont (t, _, _, _, _, _) -> "LetRec-" ^ (string_of_int t)
  | IfKont (t, _, _, _, _) -> "If-" ^ (string_of_int t)
  | SetKont (t, _, _, _) -> "Set-" ^ (string_of_int t)
  | CallccKont (t, _, _) -> "Callcc-" ^ (string_of_int t)
  | HaltKont -> "Halt"

let rec string_of_prim_value = function
  | String s -> "\"" ^ s ^ "\""
  | Integer n -> string_of_int n
  | Boolean true -> "#t"
  | Boolean false -> "#f"
  | Symbol sym -> "'" ^ sym
  | Tid t -> "#<thread " ^ (Tid.string_of_tid t) ^ ">"
  | Cons (car, cdr) ->
    "(" ^ (string_of_prim_value car) ^ " . " ^
      (string_of_prim_value cdr) ^")"
  | Nil -> "()"
  | Unspecified -> "#<unspecified>"
  | Closure ((args, body), _) ->
    "#<closure (lambda (" ^ (String.concat " "
                       (List.map fst args)) ^ ") " ^
      (Ast.string_of_nodes " " body) ^ ")>"
  | Primitive name -> "#<primitive " ^ name ^ ">"
  | Kont k -> "#<continuation " ^ (string_of_kont k) ^ ">"

let string_of_value = function
  | AbsUnique v -> string_of_prim_value v
  | AbsString -> "Str"
  | AbsInteger -> "Int"
  | AbsBoolean -> "Bool"
  | AbsSymbol -> "Sym"
  | AbsList -> "List"

(** Addresses *)

module Addr = struct
  type t = addr
  let compare = Pervasives.compare
  let is_reclaimable = function
    | PrimAddr _ -> false
    | _ -> true
  let string_of_address = function
    | TagAddr (n, t) ->
      "TagAddr(" ^ (string_of_int n) ^ "," ^ (Time.string_of_time t) ^ ")"
    | VarAddr (s, t) ->
      "VarAddr(" ^ s ^ "," ^ (Time.string_of_time t) ^ ")"
    | PrimAddr (s, t) ->
      "PrimAddr(" ^ s ^ "," ^ (Time.string_of_time t) ^ ")"
    | KontAddr (n, t) ->
      "KontAddr(" ^ (Ast.string_of_node n) ^ "," ^ (Time.string_of_time t) ^ ")"
end

module AddressSet = Set.Make(struct
    type t = addr
    let compare = Pervasives.compare
end)

let string_of_address_set set =
  "{" ^ (String.concat ", " (List.map Addr.string_of_address
                               (AddressSet.elements set))) ^ "}"

(** Some operations on primitive values and (abstract) values *)
module type AVAL =
sig
  val aval : prim_value -> value
end

module ConcreteAval = struct
  let aval v = AbsUnique v
end

module AbstractAval = struct
  let aval = function
  (* some values are directly abstracted, to avoid having infinite width
     in the value lattice *)
  | String _ -> AbsString
  | Integer _ -> AbsInteger
  | Symbol _ -> AbsSymbol
  | Cons _ -> AbsList
  | v -> AbsUnique v
end

let aval = AbstractAval.aval

let value_equals x y = compare x y = 0

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

let merge x y = match x, y with
  | AbsUnique (Primitive _), AbsUnique (Primitive _) -> None
  | AbsUnique x, AbsUnique y when value_equals x y -> Some (AbsUnique x)
  | AbsUnique (Integer _), AbsUnique (Integer _) -> Some AbsInteger
  | AbsUnique (String _), AbsUnique (String _) -> Some AbsString
  | AbsUnique (Symbol _), AbsUnique (Symbol _) -> Some AbsSymbol
  | AbsUnique (Boolean _), AbsUnique (Boolean _) -> Some AbsBoolean
  | AbsUnique (Cons _), AbsUnique (Cons _) -> Some AbsList
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

let value_not x = match x with
  | AbsUnique (Boolean b) -> Some (AbsUnique (Boolean (not b)))
  | AbsBoolean -> Some AbsBoolean
  | _ -> None
