open Address

module type STORE =
    functor (Address : ADDRESS) ->
      sig
        type +'a t
        val empty : 'a t
        val lookup : 'a t -> Address.t -> 'a
        val alloc : 'a t -> Address.t -> 'a -> 'a t
        val update : 'a t -> Address.t -> 'a -> 'a t
        val string_of_store : 'a t -> ('a -> string) -> string
      end
