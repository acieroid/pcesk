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
and time = node option (* only 0-CFA or 1-CFA *)
and addr =
  | TagAddr of int * time
  | VarAddr of string * time
  | KontAddr of node * time
and env = addr Env.t
type tag = int

let compare_time x y = match x, y with
  | None, None -> 0
  | Some n1, Some n2 -> Scheme_ast.compare_node n1 n2
  | None, Some _ -> -1
  | Some _, None -> 1

let string_of_time = function
  | Some n -> Scheme_ast.string_of_node n
  | None -> "ε"

module Addr = struct
  type t = addr
  let compare x y =
    let comp comp1 comp2 (x1, y1) (x2, y2) =
      match comp1 x1 x2 with
      | 0 -> comp2 y1 y2
      | n -> n in
    match x, y with
    | TagAddr (n1, t1), TagAddr (n2, t2) ->
      comp compare compare_time (n1, t1) (n2, t2)
    | TagAddr _, _ -> 1
    | VarAddr _, TagAddr _ -> -1
    | VarAddr _, KontAddr _ -> 1
    | VarAddr (s1, t1), VarAddr (s2, t2) ->
      comp compare compare_time (s1, t1) (s2, t2)
    | KontAddr (n1, t1), KontAddr (n2, t2) ->
      comp Scheme_ast.compare_node compare_time (n1, t1) (n2, t2)
    | KontAddr _, _ -> -1
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
let merge x y =
  print_string ("merge(" ^ (string_of_value x) ^ "," ^ (string_of_value y) ^ ")");
  match x, y with
  | AbsUnique (Primitive _), AbsUnique (Primitive _) -> None
    (* TODO: compare_kont *)
  | _ when Hashtbl.hash x = Hashtbl.hash y -> Some x
  | AbsUnique (Integer _), AbsUnique (Integer _) -> Some AbsInteger
  | AbsUnique (String _), AbsUnique (String _) -> Some AbsString
  | AbsUnique (Symbol _), AbsUnique (Symbol _) -> Some AbsSymbol
  | AbsUnique (Boolean _), AbsUnique (Boolean _) -> Some AbsBoolean
  | AbsUnique (Cons (_, _) as l1), AbsUnique (Cons (_, _) as l2) ->
    (* TODO: might fail if l1 and l2 contains a functional value (eg. a primitive) *)
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
