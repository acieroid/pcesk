module type ADDRESS =
sig
  type t
  val alloc : int -> t
  val compare : t -> t -> int
  val string_of_address : t -> string
end

