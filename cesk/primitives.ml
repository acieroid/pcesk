open Types
open Cesk_types
open Exceptions
open Cesk_base

let int_val_op (name : string) (f : int list -> value) =
  name,
  fun args ->
    let ns = List.map (fun arg -> match arg with
        | Integer n -> n
        | _ -> raise (PrimWrongArgType (name, arg))) args in
    (f ns)

let int_op name (f : int list -> int) : prim =
  int_val_op name (fun ns -> Integer (f ns))

let int_comp name (f : int list -> bool) : prim =
  int_val_op name (fun ns -> Boolean (f ns))

let rec cmp op = function
  | [] | [_] -> true
  | x :: y :: rest ->
    if op x y then
      cmp op (y :: rest)
    else
      false

let primitives : prim list =
  [int_op "+" (fun ns -> (List.fold_left (+) 0 ns));
   int_op "-" (function
     | [] -> raise (PrimWrongNumberOfArgs ("-", 0))
     | [x] -> -x
     | hd :: tl -> List.fold_left (-) hd tl);
   int_op "*" (fun ns -> (List.fold_left ( * ) 1 ns));
   int_comp "=" (cmp (=));
   int_comp ">" (cmp (>));
   int_comp "<" (cmp (<));
   int_comp ">=" (cmp (>=));
   int_comp "<=" (cmp (<=));
]

let apply_primitive ((name, f) : prim) (args : value list) : value =
  f args

let install_primitives (state : state) : state =
  let inst state ((name, _) as prim) =
    let a = alloc_prim state name in
    {state with
     env = env_extend state.env name a;
     store = store_extend state.store a (Lattice.abst1 (Primitive prim));
     time = tick state}
  in
  List.fold_left inst state primitives
