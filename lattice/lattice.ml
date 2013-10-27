open Types

module type LATTICE =
sig
  type t
  val abst1 : value -> t
  val abst : value list -> t
  val join : t -> t -> t
  val string_of_lattice_value : t -> string

  val op_int_bin : (int -> int -> int) -> t -> t -> t
  val op_int_un : (int -> int) -> t -> t
  val op_int_comp : (int -> int -> bool) -> t -> t -> t
  val op_eq : t -> t -> t
  val op_neq : t -> t -> t

  val test : unit -> unit
end
