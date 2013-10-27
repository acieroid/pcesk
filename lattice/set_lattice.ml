open Types
open Lattice
open OUnit

module type SIZE =
sig
  val size : int
end

(** TODO: use set instead of list (but it requires a compare function for values) *)
module Set_lattice : functor (Size : SIZE) -> LATTICE =
  functor (Size : SIZE) ->
  struct

    let product l1 l2 =
      let rec product' l1 l2 acc = match l1 with
        | [] -> acc
        | h :: t -> product' t l2 ((List.map (fun x -> (h, x)) l2) @ acc)
      in product' l1 l2 []

    type t =
      | Values of value list
      | Top
      | Bot

    let abst1 v = Values [v]

    let abst vs =
      match vs with
      | [] -> Bot
      | _ ->
        if List.length vs > Size.size then
          Top
        else
          Values vs

    let join x y = match x, y with
      | Bot, x | x, Bot -> x
      (* TODO: union, not concatenation *)
      | Values vs1, Values vs2 -> abst (vs1 @ vs2)
      | _ -> Top

    let string_of_lattice_value = function
      | Values vs -> "[" ^ (String.concat ", " (List.map string_of_value vs)) ^ "]"
      | Top -> "Top"
      | Bot -> "Bot"

    let op_bin f x y = match x, y with
      | Bot, _ | _, Bot -> Bot
      | Values vs1, Values vs2 ->
        abst (List.map (fun (x, y) -> f x y) (product vs1 vs2))
      | _ -> Top

    let op_un f x = match x with
      | Bot -> Bot
      | Values vs -> Values (List.map f vs)
      | _ -> Top

    let test () =
      assert_equal ~msg:"5" (abst1 (Integer 5)) (Values [Integer 5]);
      assert_equal ~msg:"5,6" (abst [Integer 5; Integer 6])
        (Values [Integer 5; Integer 6]);
      assert_equal ~msg:"5,foo" (abst [Integer 5; String "foo"])
        (Values [Integer 5; String "foo"]);
      assert_equal ~msg:"5,'foo" (abst [Integer 5; Symbol "foo"])
        (Values [Integer 5; Symbol "foo"]);
      assert_equal ~msg:"[]" (abst []) Bot;

      (* Join *)
      assert_equal ~msg:"5,6" (join (abst1 (Integer 5)) (abst1 (Integer 6)))
        (Values [Integer 5; Integer 6])

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

      (* TODO: more tests, and dependent on Size.size *)

  end
