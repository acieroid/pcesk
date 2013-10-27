open Types

module type LATTICE =
sig
  type t
  val abst1 : value -> t
  val abst : value list -> t
  val join : t -> t -> t
  val string_of_lattice_value : t -> string

  val op_bin : (value -> value -> value) -> t -> t -> t
  val op_un : (value -> value) -> t -> t

  val test : unit -> unit
end
