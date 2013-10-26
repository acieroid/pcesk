open Address

module type ENV =
    functor (Addr : ADDRESS) ->
      sig
        type t
        val empty : t
        val lookup : t -> string -> Addr.t
        val extend : t -> string -> Addr.t -> t
        val string_of_env : t -> string
      end
