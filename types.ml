open Env

(** Types *)

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
  | OperatorKont of int * node list * env * addr
  | OperandsKont of int * value * node list * value list * env * addr
  | BeginKont of int * node list * env * addr
  | DefineKont of int * string * env * addr
  | IfKont of int * node * node * env * addr
  | SetKont of int * string * env * addr
  | HaltKont
and prim = string * (value list -> value option)
and time = node option (* only 0-CFA or 1-CFA *)
and addr =
  | TagAddr of int * time
  | VarAddr of string * time
  | KontAddr of node * time
and env = addr Env.t
type tag = int

let string_of_time = function
  | Some n -> Scheme_ast.string_of_node n
  | None -> "ε"

module Addr = struct
  type t = addr
  let compare = Pervasives.compare
  let string_of_address = function
    | TagAddr (n, t) ->
      "TagAddr(" ^ (string_of_int n) ^ "," ^ (string_of_time t) ^ ")"
    | VarAddr (s, t) ->
      "VarAddr(" ^ s ^ "," ^ (string_of_time t) ^ ")"
    | KontAddr (n, t) ->
      "KontAddr(" ^ (Scheme_ast.string_of_node n) ^ "," ^ (string_of_time t) ^ ")"
end

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
  | Primitive name -> "#<primitive " ^ name ^ ">"
  | Kont k -> "#<continuation " ^ (string_of_kont k) ^ ">"

let string_of_value = function
  | AbsUnique v -> string_of_prim_value v
  | AbsString -> "Str"
  | AbsInteger -> "Int"
  | AbsBoolean -> "Bool"
  | AbsSymbol -> "Sym"
  | AbsList -> "List"

(** Some operations on abstract values *)
(* TODO: is probably wrong on certain cases *)
let kont_equals x y = (Hashtbl.hash x) = (Hashtbl.hash y) && match x, y with
  | OperatorKont (n1, ns1, e1, a1), OperatorKont (n2, ns2, e2, a2) ->
    n1 = n2
  | OperandsKont (n1, v1, ns1, vs1, e1, a1),
    OperandsKont (n2, v2, ns2, vs2, e2, a2) ->
    n1 = n2
  | BeginKont (n1, ns1, e1, a1), BeginKont (n2, ns2, e2, a2) ->
    n1 = n2
  | DefineKont (n1, s1, e1, a1), DefineKont (n2, s2, e2, a2) ->
    n1 = n2
  | SetKont (n1, s1, e1, a1), SetKont (n2, s2, e2, a2) ->
    n1 = n2
  | HaltKont, HaltKont -> true
  | _ -> false

let value_equals x y = compare x y = 0

let merge x y =
  print_string ("merge(" ^ (string_of_value x) ^ "," ^ (string_of_value y) ^ ")");
  match x, y with
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
