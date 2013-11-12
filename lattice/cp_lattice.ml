open Types
open Lattice
open OUnit2

module Cp_lattice : LATTICE =
struct

type t =
  | Unique of value
  | Top
  | Bot

let is_bottom = function
  | Bot -> true
  | _ -> false

let abst1 v = Unique v

let abst vs = match vs with
  | [] -> Bot
  | [v] -> Unique v
  | _ -> Top

let conc = function
  | Unique v -> [v]
  | Bot -> []
  | Top -> raise TooAbstracted

let meet x y = match x, y with
  | Top, v | v, Top -> v
  | Bot, _ | _, Bot -> Bot
  | Unique v1, Unique v2 when v1 = v2 -> Unique v1
  | _ -> Bot

let join x y = match x, y with
  | Bot, x | x, Bot -> x
  | Unique v1, Unique v2 when v1 = v2 -> Unique v1
  | _ -> Top

let string_of_lattice_value = function
  | Top -> "Top"
  | Bot -> "Bot"
  | Unique v -> "Unique(" ^ (string_of_value v) ^ ")"

let op_bin f x y = match x, y with
  | Bot, _ | _, Bot -> Bot
  | Unique v1, Unique v2 -> Unique (f v1 v2)
  | _ -> Top

let op_un f = function
  | Bot -> Bot
  | Unique v -> Unique (f v)
  | _ -> Top

let test ctx =
  assert_equal ~msg:"5" (abst1 (Integer 5)) (Unique (Integer 5));
  assert_equal ~msg:"5,6" (abst [Integer 5; Integer 6]) Top;
  assert_equal ~msg:"[]" (abst []) Bot;

  let six = abst1 (Integer 6)
  and seven = abst1 (Integer 7)
  and str = abst1 (String "foo") in
  assert_equal ~msg:"6,6" (join six six) six;
  assert_equal ~msg:"6,7" (join six seven) Top;

  assert_equal ~msg:"6*7" (op_bin value_mul six seven) (abst1 (Integer 42));

  assert_equal ~msg:"-6" (op_un value_neg six) (abst1 (Integer (-6)));

  assert_equal ~msg:"6>7" (op_bin value_gt six seven) (abst1 (Boolean false));
  assert_equal ~msg:"6<7" (op_bin value_lt six seven) (abst1 (Boolean true));

  assert_equal ~msg:"6=6" (op_bin value_eq six six) (abst1 (Boolean true));
  assert_equal ~msg:"6=7" (op_bin value_eq six seven) (abst1 (Boolean false));
  assert_equal ~msg:"6=foo" (op_bin value_eq six str) (abst1 (Boolean false));

end
