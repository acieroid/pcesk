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
      List.concat (List.map (fun y -> f y x) acc))
    [init] ns

let int_comp f = function
  | [] | [_] -> [Aval.aval (Boolean true)]
  | hd :: tl -> int_op f hd tl

let cons = function
  | car :: cdr :: [] -> [Aval.aval (Cons (car, cdr))]
  | _ -> []

let car = function
  | l :: [] ->
    begin match l with
      | AbsUnique (Cons (car, _)) -> [car]
      | _ -> []
    end
  | _ -> []

let cdr = function
  | l :: [] ->
    begin match l with
      | AbsUnique (Cons (_, cdr)) -> [cdr]
      | _ -> []
    end
  | _ -> []

let emptyp = function
  | l :: [] ->
    begin match l with
      | AbsUnique Nil -> [Aval.aval (Boolean true)]
      | AbsUnique (Cons _) -> [Aval.aval (Boolean false)]
      | _ -> []
    end
  | _ -> []

let primitives : prim list =
  [("+", int_op value_add (AbsUnique (Integer 0)));
   ("-", function
       | [] -> []
       | [x] -> value_neg x
       | hd :: tl -> int_op value_sub hd tl);
   ("*", int_op value_mul (AbsUnique (Integer 1)));
   ("/", function
       | [] -> []
       | [x] -> value_div (AbsUnique (Integer 1)) x
       | hd :: tl -> int_op value_div hd tl);
   ("modulo", function
       | x :: y :: [] -> value_mod x y
       | _ -> []);
   ("=", int_comp value_int_eq);
   (">", int_comp value_gt);
   (">=", int_comp value_gte);
   ("<", int_comp value_lt);
   ("<=", int_comp value_lte);
   ("not", function
       | [hd] -> value_not hd
       | _ -> []);
   ("cons", cons);
   ("car", car);
   ("cdr", cdr);
   ("empty?", emptyp);
  ]

let apply_primitive (name : string) (args : value list) : value list =
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
