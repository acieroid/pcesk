open Types

module type AVAL =
sig
  val aval : prim_value -> value
end

module ConcreteAval = struct
  let aval v = AbsUnique v
end

module AbstractAval = struct
  let rec abstract_cons car cdr length =
    if length = 0 then
      AbsList
    else match cdr with
      | AbsUnique (Cons (cdar, cddr)) ->
        AbsUnique (Cons (car, abstract_cons cdar cddr (length-1)))
      | _ -> AbsUnique (Cons (car, cdr))
  let aval = function
  (* some values are directly abstracted, to avoid having infinite width
     in the value lattice *)
  | String _ -> AbsString
  | Integer _ -> AbsInteger
  | Symbol _ -> AbsSymbol
  | Cons (car, cdr) as cons -> abstract_cons car cdr !Params.list_length
  | v -> AbsUnique v
end
