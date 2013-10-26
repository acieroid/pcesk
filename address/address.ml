module type ADDRESS =
  sig
    type t
    val first : t
    val next : t -> t
    val compare : t -> t -> int
    val string_of_address : t -> string
  end

