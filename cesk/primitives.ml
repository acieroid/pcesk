open Types
open Cesk_types
open Exceptions
open Cesk_base

let rec cmp op = function
  | [] | [_] -> true
  | x :: y :: rest ->
    if op x y then
      cmp op (y :: rest)
    else
      false

let int_op f init ns =
  List.fold_left (fun acc x ->
      match acc with
      | Some y -> f y x
      | None -> None) (Some init) ns

let int_comp f = function
  | [] | [_] -> Some (AbsUnique (Boolean true))
  | hd :: tl -> int_op f hd tl

let primitives : prim list =
  [("+", int_op value_add (AbsUnique (Integer 0)));
   ("-", function
       | [] -> None
       | [x] -> value_neg x
       | hd :: tl -> int_op value_sub hd tl);
   ("*", int_op value_mul (AbsUnique (Integer 1)));
   ("=", int_comp value_int_eq);
   (">", int_comp value_gt);
   (">=", int_comp value_gte);
   ("<", int_comp value_lt);
   ("<=", int_comp value_lte);
  ]

let apply_primitive ((name, f) : prim) (args : value list) : value option =
  f args

let install_primitives (state : state) : state =
  let inst state ((name, _) as prim) =
    let a = alloc_prim state name in
    {state with
     env = env_extend state.env name a;
     store = store_extend state.store a (Lattice.abst1 (AbsUnique (Primitive prim)));
     time = tick state}
  in
  List.fold_left inst state primitives
