module type ADDRESS =
sig
  type t
  val alloc : int -> t
  val alloc_prim : string -> t
  val alloc_kont : int  -> t
  val compare : t -> t -> int
  val string_of_address : t -> string
end

module Mono_addr : ADDRESS =
struct

  type t =
    | ValAddr of int
    | KontAddr of int
    | PrimAddr of string

  let alloc tag = ValAddr tag

  let alloc_prim tag = PrimAddr tag

  let alloc_kont tag = KontAddr tag

  let compare x y = match x, y with
    | ValAddr a, ValAddr b
    | KontAddr a, KontAddr b -> Pervasives.compare a b
    | PrimAddr a, PrimAddr b -> Pervasives.compare a b
    | ValAddr _, _ -> 1
    | KontAddr _, ValAddr _ -> -1
    | KontAddr _, PrimAddr _ -> 1
    | PrimAddr _, _ -> -1

  let string_of_address = function
    | ValAddr a -> "val" ^ (string_of_int a)
    | KontAddr a -> "kont" ^ (string_of_int a)
    | PrimAddr a -> "prim" ^ a

end
