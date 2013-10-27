open Types
open Lattice
open OUnit

module Cp_lattice : LATTICE =
struct

type t =
  | Unique of value
  | Top
  | Bot

let abst1 v = Unique v

let abst vs = match vs with
  | [] -> Bot
  | [v] -> Unique v
  | _ -> Top

let join x y = match x, y with
  | Bot, x | x, Bot -> x
  | Unique v1, Unique v2 when v1 = v2 -> Unique v1
  | _ -> Top

let string_of_lattice_value = function
  | Top -> "Top"
  | Bot -> "Bot"
  | Unique v -> "Unique(" ^ (string_of_value v) ^ ")"

let op_int_bin f x y = match x, y with
  | Bot, _ | _, Bot -> Bot
  | Unique (Integer n1), Unique (Integer n2) -> Unique (Integer (f n1 n2))
  | _ -> Top

let op_int_un f x = match x with
  | Bot -> Bot
  | Unique (Integer n) -> Unique (Integer (f n))
  | _ -> Top

let op_int_comp f x y = match x, y with
  | Bot, _ | _, Bot -> Bot
  | Unique (Integer n1), Unique (Integer n2) -> Unique (Boolean (f n1 n2))
  | _ -> Top

let op_eq x y = match x, y with
  | Bot, _ | _, Bot -> Bot
  | Unique v1, Unique v2 -> Unique (Boolean (v1 = v2))
  | _ -> Top

let op_neq x y = match x, y with
  | Bot, _ | _, Bot -> Bot
  | Unique v1, Unique v2 -> Unique (Boolean (v1 <> v2))
  | _ -> Top

let test () =
  assert_equal ~msg:"5" (abst1 (Integer 5)) (Unique (Integer 5));
  assert_equal ~msg:"5,6" (abst [Integer 5; Integer 6]) Top;
  assert_equal ~msg:"[]" (abst []) Bot;

  let six = abst1 (Integer 6)
  and seven = abst1 (Integer 7)
  and str = abst1 (String "foo") in
  assert_equal ~msg:"6,6" (join six six) six;
  assert_equal ~msg:"6,7" (join six seven) Top;

  assert_equal ~msg:"6*7" (op_int_bin ( * ) six seven) (abst1 (Integer 42));
  assert_equal ~msg:"6+foo" (op_int_bin (+) six str) Top;

  assert_equal ~msg:"-6" (op_int_un (fun x -> -x) six) (abst1 (Integer (-6)));

  assert_equal ~msg:"6>7" (op_int_comp (>) six seven) (abst1 (Boolean false));
  assert_equal ~msg:"6<7" (op_int_comp (<) six seven) (abst1 (Boolean true));

  assert_equal ~msg:"6=6" (op_eq six six) (abst1 (Boolean true));
  assert_equal ~msg:"6=7" (op_eq six seven) (abst1 (Boolean false));
  assert_equal ~msg:"6=foo" (op_eq six str) (abst1 (Boolean false));

end
