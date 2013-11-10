open Types

module type ADDRESS =
sig
  type t
  val compare : t -> t -> int
  val is_reclaimable : t -> bool
  val string_of_address : t -> string
end
