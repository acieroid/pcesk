open Types

exception TooAbstracted

module type LATTICE =
sig
  (* An element of the lattice *)
  type t

  (* Check if this lattice value is the bottom one *)
  val is_bottom : t -> bool

  (* Abstract one value *)
  val abst1 : value -> t

  (* Abstract a list of values *)
  val abst : value list -> t

  (* Join two lattice elements together *)
  val join : t -> t -> t

  (* Return the list of values corresponding to this lattice element.
     If elements are too abstracted to be extracted, raise a
     TooAbstracted exception *)
  val conc : t -> value list

  (* Compute the intersection of the first lattice value with the
     second one *)
  val meet : t -> t -> t

  (* Return a string representation of a lattice element *)
  val string_of_lattice_value : t -> string

  (* Performs a binary operation on a lattice element *)
  val op_bin : (value -> value -> value option) -> t -> t -> t

  (* Performs an unary operation on a lattice element *)
  val op_un : (value -> value option) -> t -> t

  (* Run unit tests on the lattice (see OUnit) *)
  val test : OUnit2.test_ctxt -> unit
end
