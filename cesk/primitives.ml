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
  | [] | [_] -> Some (Aval.aval (Boolean true))
  | hd :: tl -> int_op f hd tl

let cons = function
  (* TODO: abstract lists in some useful way *)
  (* TODO: use aval *)
  | car :: cdr :: [] -> Some (Aval.aval (Cons (car, cdr)))
  | _ -> None

let car = function
  | l :: [] ->
    begin match l with
      | AbsUnique (Cons (car, _)) -> Some car
      | _ -> None
    end
  | _ -> None

let cdr = function
  | l :: [] ->
    begin match l with
      | AbsUnique (Cons (_, cdr)) -> Some cdr
      | _ -> None
    end
  | _ -> None

let emptyp = function
  | l :: [] ->
    begin match l with
      (* TODO: use aval *)
      | AbsUnique Nil -> Some (Aval.aval (Boolean true))
      | AbsUnique (Cons _) -> Some (Aval.aval (Boolean false))
      | _ -> None
    end
  | _ -> None

let primitives : prim list =
  [("+", int_op value_add (AbsUnique (Integer 0)));
   ("-", function
       | [] -> None
       | [x] -> value_neg x
       | hd :: tl -> int_op value_sub hd tl);
   ("*", int_op value_mul (AbsUnique (Integer 1)));
   ("/", function
       | [] -> None
       | [x] -> value_div (AbsUnique (Integer 1)) x
       | hd :: tl -> int_op value_div hd tl);
   ("modulo", function
       | x :: y :: [] -> value_mod x y
       | _ -> None);
   ("=", int_comp value_int_eq);
   (">", int_comp value_gt);
   (">=", int_comp value_gte);
   ("<", int_comp value_lt);
   ("<=", int_comp value_lte);
   ("not", function
     | [hd] -> value_not hd
     | _ -> None);
   ("cons", cons);
   ("car", car);
   ("cdr", cdr);
   ("empty?", emptyp);
  ]

let apply_primitive (name : string) (args : value list) : value option =
  try
    (List.assoc name primitives) args
  with
    Not_found -> raise (UnboundIdentifier name)

let install_primitives (state : state) : state =
  let inst state (name, _) =
    let a = alloc_prim state name in
    {state with
     env = env_extend state.env name a;
     store = store_extend1 state.store a (AbsUnique (Primitive name))}
  in
  List.fold_left inst state primitives
