open Types

module type AVAL =
sig
  val aval : prim_value -> value
end

module ConcreteAval = struct
  let aval v = AbsUnique v
end

module AbstractAval = struct
  let aval = function
  (* some values are directly abstracted, to avoid having infinite width
     in the value lattice *)
  | String _ -> AbsString
  | Integer _ -> AbsInteger
  | Symbol _ -> AbsSymbol
  | Cons _ -> AbsList
  | v -> AbsUnique v
end
