open Types
open Lattice
open OUnit

module Lattice1 : LATTICE =
struct

type t =
  | Unique of value
  | Num
  | Str
  | NumStr
  | Bool
  | Top
  | Bot

let abst1 v = Unique v

let join x y = match x, y with
  | Bot, x | x, Bot -> x
  | Top, _ | _, Top -> Top
  | Unique v1, Unique v2 when v1 = v2 -> Unique v1
  | Unique (Integer _), Unique (Integer _) -> Num
  | Unique (String _), Unique (String _) -> Str
  | Unique (Integer _), Unique (String _)
  | Unique (String _), Unique (Integer _) -> NumStr
  | Unique (Boolean _), Unique (Boolean _) -> Bool
  | Num, Num -> Num
  | Str, Str -> Str
  | Num, Str | Str, Num -> NumStr
  | Bool, Bool -> Bool
  | _ -> Top

let abst vs =
  List.fold_left join Bot (List.map abst1 vs)

let string_of_lattice_value = function
  | Unique v -> "Unique(" ^ (string_of_value v) ^ ")"
  | Num -> "Num"
  | Str -> "Str"
  | NumStr -> "NumStr"
  | Bool -> "Bool"
  | Top -> "Top"
  | Bot -> "Bot"

let op_int_bin f x y = match x, y with
  | Bot, _ | _, Bot -> Bot
  | Unique (Integer n1), Unique (Integer n2) -> Unique (Integer (f n1 n2))
  | Unique (Integer _), Num | Num, Unique (Integer _) -> Num
  | Num, Num -> Num
  | _ -> Top

let op_int_un f x = match x with
  | Bot -> Bot
  | Unique (Integer n1) -> Unique (Integer (f n1))
  | Num -> Num
  | _ -> Top

let op_int_comp f x y = match x, y with
  | Bot, _ | _, Bot -> Bot
  | Unique (Integer n1), Unique (Integer n2) -> Unique (Boolean (f n1 n2))
  | Unique (Integer _), Num | Num, Unique (Integer _) | Num, Num -> Bool
  | _ -> Top

let op_eq x y = match x, y with
  | Bot, _ | _, Bot -> Bot
  | Unique v1, Unique v2 -> Unique (Boolean (v1 = v2))
  | Unique (Integer _), Num | Num, Unique (Integer _) -> Bool
  | Unique (String _), Str | Str, Unique (String _) -> Bool
  | Num, Num | Str, Str | Bool, Bool -> Bool
  | _ -> Top

let op_neq x y = match x, y with
  | Bot, _ | _, Bot -> Bot
  | Unique v1, Unique v2 -> Unique (Boolean (v1 <> v2))
  | Num, Num | Str, Str | Bool, Bool -> Bool
  | _ -> Top

let test () =
  (* Abstraction *)
  assert_equal ~msg:"5" (abst1 (Integer 5)) (Unique (Integer 5));
  assert_equal ~msg:"5,6" (abst [Integer 5; Integer 6]) Num;
  assert_equal ~msg:"5,foo" (abst [Integer 5; String "foo"]) NumStr;
  assert_equal ~msg:"5,'foo" (abst [Integer 5; Symbol "foo"]) Top;
  assert_equal ~msg:"[]" (abst []) Bot;

  (* Join *)
  assert_equal ~msg:"Num,Num" (join Num Num) Num;
  assert_equal ~msg:"Num,Str" (join Num Str) NumStr;

  (* Operations *)
  let six = abst1 (Integer 6)
  and seven = abst1 (Integer 7)
  and str = abst1 (String "foo") in
  assert_equal ~msg:"6*7" (op_int_bin ( * ) six seven) (abst1 (Integer 42));
  assert_equal ~msg:"6+foo" (op_int_bin (+) six str) Top;

  assert_equal ~msg:"-6" (op_int_un (fun x -> -x) six) (abst1 (Integer (-6)));

  assert_equal ~msg:"6>7" (op_int_comp (>) six seven) (abst1 (Boolean false));
  assert_equal ~msg:"6<7" (op_int_comp (<) six seven) (abst1 (Boolean true));

  assert_equal ~msg:"6=6" (op_eq six six) (abst1 (Boolean true));
  assert_equal ~msg:"6=7" (op_eq six seven) (abst1 (Boolean false));
  assert_equal ~msg:"6=Num" (op_eq six Num) Bool

end
