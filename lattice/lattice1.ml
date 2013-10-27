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

let op_bin f x y = match x, y with
  | Bot, _ | _, Bot -> Bot
  | Unique v1, Unique v2 -> Unique (f v1 v2)
  | _ -> Top

let op_un f x = match x with
  | Bot -> Bot
  | Unique v -> Unique (f v)
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
  and seven = abst1 (Integer 7) in
  assert_equal ~msg:"6*7" (op_bin value_mul six seven) (abst1 (Integer 42));
  assert_equal ~msg:"6+foo" (op_bin value_mul six Top) Top;

  assert_equal ~msg:"-6" (op_un value_neg six) (abst1 (Integer (-6)));

  assert_equal ~msg:"6>7" (op_bin value_gt six seven) (abst1 (Boolean false));
  assert_equal ~msg:"6<7" (op_bin value_lt six seven) (abst1 (Boolean true));

  assert_equal ~msg:"6=6" (op_bin value_eq six six) (abst1 (Boolean true));
  assert_equal ~msg:"6=7" (op_bin value_eq six seven) (abst1 (Boolean false));
  assert_equal ~msg:"6=Num" (op_bin value_eq six Num) Top

end
