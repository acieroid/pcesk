open Types

exception PrimWrongArgType of string * value

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
   int_op "-" (fun ns -> (List.fold_left (-) 0 ns));
   int_op "*" (fun ns -> (List.fold_left ( * ) 1 ns));
   int_comp "=" (cmp (=));
   int_comp ">" (cmp (>));
   int_comp "<" (cmp (<));
   int_comp ">=" (cmp (>=));
   int_comp "<=" (cmp (<=));
]

