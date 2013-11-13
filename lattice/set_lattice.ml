open Types
open Lattice
open OUnit2

(** TODO: since the number of possible abstract values in the same lattice
    elements will always be bounded (assuming that the number of closures,
    primitives and continuations is bounded), we probably don't need to
    bound the size of the set lattice *)
module type SIZE =
sig
  val size : int
end

(** TODO: use set instead of list (but it requires a compare function
    for values) *)
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

    let bottom = Bot

    let is_bottom = function
      | Bot -> true
      | _ -> false

    let abst1 v = Values [v]

    let join x y =
      let abst_nocheck = function
        | [] -> Bot
        | vs ->
          if List.length vs > Size.size then
            Top
          else
            Values vs in
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
      | Values vs1, Values vs2 -> abst_nocheck (merge_values vs1 vs2)
      | _ -> Top

    let abst vs =
      List.fold_left join Bot (List.map abst1 vs)

    let conc = function
      | Values vs -> vs
      | Bot -> []
      | Top -> raise TooAbstracted

    let meet x y = match x, y with
      | Top, v | v, Top -> v
      | Bot, _ | _, Bot -> Bot
      | Values v1, Values v2 ->
        abst (List.filter
                (fun x ->
                   try
                     let _ = List.find (fun y -> value_subsumes x y) v2 in
                     true
                   with
                     Not_found -> false) v1)

    let string_of_lattice_value = function
      | Values vs -> "[" ^ (String.concat ", " (List.map string_of_value vs)) ^ "]"
      | Top -> "Top"
      | Bot -> "Bot"

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

    let test ctx =
      let test name = assert_equal ~printer:string_of_lattice_value ~msg:name in
      let five = AbsUnique (Integer 5)
      and six = AbsUnique (Integer 6)
      and str = AbsUnique (String "foo") in
      test "5" (Values [five]) (abst1 five);
      test "5,6" (Values [AbsInteger]) (abst [five; six]);
      test "5,\"foo\"" (Values [five; str]) (abst [five; str]);
      test "[]" Bot (abst []);

      (* Join *)
      test "join(5,6)" (Values [AbsInteger]) (join (abst1 five) (abst1 six));

      (* Operations *)
      let abs_six = abst1 six
      and abs_seven = abst1 (AbsUnique (Integer 7))
      and abs_fortytwo = abst1 (AbsUnique (Integer 42))
      and abs_minus_six = abst1 (AbsUnique (Integer (-6)))
      and abs_true = abst1 (AbsUnique (Boolean true))
      and abs_false = abst1 (AbsUnique (Boolean false)) in
      test "6*7" abs_fortytwo (op_bin value_mul abs_six abs_seven);
      test "6+foo" Top (op_bin value_mul abs_six Top);

      test "-6" abs_minus_six (op_un value_neg abs_six);

      test "6>7" abs_false (op_bin value_gt abs_six abs_seven);
      test "6<7" abs_true (op_bin value_lt abs_six abs_seven);

      test "6=6" abs_true (op_bin value_int_eq abs_six abs_six);
      test "6=7" abs_false (op_bin value_int_eq abs_six abs_seven);

      (* Meet *)
      test "meet(6,7)" Bot (meet abs_six abs_seven);
      test "meet(6, \"foo\")" Bot (meet abs_six (abst1 str));
      test "meet(#t, #f)" Bot (meet abs_true abs_false);
      test "meet(Bool, #f)" (abst1 AbsBoolean) (meet (abst1 AbsBoolean) abs_false);
      test "meet(#t, Bool)" Bot (meet abs_true (abst1 AbsBoolean));

  end
