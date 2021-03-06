open Env

(** Types *)

type lam = (string * int) list * (Ast.node list)
type prim_value =
  | String of string
  | Integer of int
  | Boolean of bool
  | Symbol of string
  | Tid of tid
  | Cons of value * value
  | Unlocked
  | Locked
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
and time =
  | IntTime of int
  | KCallSitesTime of Ast.node list
and addr =
  | HaltAddr
  | TagAddr of int * time
  | VarAddr of string * time
  | PrimAddr of string * time
  | KontAddr of Ast.node * time
  | TidAddr of tid
and tid =
  | InitialTid
  | TagTid of int * time
  | IntTid of int
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

let string_of_time = function
  | IntTime n -> string_of_int n
  | KCallSitesTime l -> "[" ^ (String.concat ","
                                 (List.map Ast.string_of_node l)) ^ "]"

let string_of_tid = function
  | InitialTid -> "1"
  | IntTid n -> (string_of_int n)
  | TagTid (n, t) -> (string_of_int n) ^ (string_of_time t)

let rec string_of_prim_value = function
  | String s -> "\"" ^ s ^ "\""
  | Integer n -> string_of_int n
  | Boolean true -> "#t"
  | Boolean false -> "#f"
  | Symbol sym -> "'" ^ sym
  | Tid t -> "#<thread " ^ (string_of_tid t) ^ ">"
  | Cons (car, cdr) ->
    "(" ^ (string_of_value car) ^ " . " ^
    (string_of_value cdr) ^")"
  | Locked -> "#locked"
  | Unlocked -> "#unlocked"
  | Nil -> "()"
  | Unspecified -> "#<unspecified>"
  | Closure ((args, body), _) ->
    "#<closure (lambda (" ^ (String.concat " "
                               (List.map fst args)) ^ ") " ^
    (Ast.string_of_nodes " " body) ^ ")>"
  | Primitive name -> "#<primitive " ^ name ^ ">"
  | Kont k -> "#<continuation " ^ (string_of_kont k) ^ ">"

and string_of_value = function
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
    | PrimAddr _
    | TidAddr _ -> false
    | _ -> true
  let string_of_address = function
    | HaltAddr ->
      "HaltAddr"
    | TagAddr (n, t) ->
      "TagAddr(" ^ (string_of_int n) ^ "," ^ (string_of_time t) ^ ")"
    | VarAddr (s, t) ->
      "VarAddr(" ^ s ^ "," ^ (string_of_time t) ^ ")"
    | PrimAddr (s, t) ->
      "PrimAddr(" ^ s ^ "," ^ (string_of_time t) ^ ")"
    | KontAddr (n, t) ->
      "KontAddr(" ^ (Ast.string_of_node n) ^ "," ^ (string_of_time t) ^ ")"
    | TidAddr t ->
      "TidAddr(" ^ (string_of_tid t) ^ ")"
end

module AddressSet = Set.Make(struct
    type t = addr
    let compare = Pervasives.compare
  end)

let string_of_address_set set =
  "{" ^ (String.concat ", " (List.map Addr.string_of_address
                               (AddressSet.elements set))) ^ "}"

(** Some operations on primitive values and (abstract) values *)

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
let value_mod = value_op_int (mod)

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
