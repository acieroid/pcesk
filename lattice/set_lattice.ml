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

    let is_bottom = function
      | Bot -> true
      | _ -> false

    let abst1 v = Values [v]

    let abst = function
      | [] -> Bot
      | vs ->
        if List.length vs > Size.size then
          Top
        else
          Values vs

    let conc = function
      | Values vs -> vs
      | Bot -> []
      | Top -> raise TooAbstracted

    let meet x y = match x, y with
      | Top, v | v, Top -> v
      | Bot, _ | _, Bot -> Bot
      | Values v1, Values v2 -> abst (List.filter (fun x -> List.mem x v2) v1)

    let string_of_lattice_value = function
      | Values vs -> "[" ^ (String.concat ", " (List.map string_of_value vs)) ^ "]"
      | Top -> "Top"
      | Bot -> "Bot"

    let join x y =
      let rec merge_value v = function
        | [] -> [v]
        | hd :: tl ->
          begin match merge v hd with
          | Some v' -> v' :: tl
          | None -> hd :: (merge_value v tl)
          end in
      let merge_values vs1 vs2 =
        List.fold_left (fun l v -> merge_value v l) vs1 vs2 in
      match x, y with
      | Bot, x | x, Bot -> x
      | Values vs1, Values vs2 -> abst (merge_values vs1 vs2)
      | _ -> Top

    let filter_option l =
      BatList.filter_map (fun x -> x) l

    let op_bin f x y = match x, y with
      | Bot, _ | _, Bot -> Bot
      | Values vs1, Values vs2 ->
        abst (filter_option (List.map (fun (x, y) -> f x y) (product vs1 vs2)))
      | _ -> Top

    let op_un f x = match x with
      | Bot -> Bot
      | Values vs -> Values (filter_option (List.map f vs))
      | _ -> Top

    let test () =
      let five = AbsUnique (Integer 5)
      and six = AbsUnique (Integer 6)
      and str = AbsUnique (String "foo") in
      assert_equal ~msg:"5" (abst1 five) (Values [five]);
      assert_equal ~msg:"5,6" (abst [five; six])
        (Values [AbsInteger]);
      assert_equal ~msg:"5,foo" (abst [five; str])
        (Values [five; str]);
      assert_equal ~msg:"[]" (abst []) Bot;

      (* Join *)
      assert_equal ~msg:"5,6" (join (abst1 five) (abst1 six))
        (Values [AbsInteger]);

      (* Operations *)
      let abs_six = abst1 six
      and abs_seven = abst1 (AbsUnique (Integer 7))
      and abs_fortytwo = abst1 (AbsUnique (Integer 42))
      and abs_minus_six = abst1 (AbsUnique (Integer (-6)))
      and abs_true = abst1 (AbsUnique (Boolean true))
      and abs_false = abst1 (AbsUnique (Boolean false)) in
      assert_equal ~msg:"6*7" (op_bin value_mul abs_six abs_seven) abs_fortytwo;
      assert_equal ~msg:"6+foo" (op_bin value_mul abs_six Top) Top;

      assert_equal ~msg:"-6" (op_un value_neg abs_six) abs_minus_six;

      assert_equal ~msg:"6>7" (op_bin value_gt abs_six abs_seven) abs_false;
      assert_equal ~msg:"6<7" (op_bin value_lt abs_six abs_seven) abs_true;

      assert_equal ~msg:"6=6" (op_bin value_int_eq abs_six abs_six) abs_true;
      assert_equal ~msg:"6=7" (op_bin value_int_eq abs_six abs_seven) abs_false;

      (* TODO: more tests, and dependent on Size.size *)

  end
