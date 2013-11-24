module type TID = sig
  type t
  val initial : t
  val compare : t -> t -> int
  val max : t list -> t
  val next : t -> t
  val string_of_tid : t -> string
end

module ConcreteTID = struct
  type t = int
  let initial = 1
  let compare = Pervasives.compare
  let max = BatList.max
  let next t = t+1
  let string_of_tid = string_of_int
end
