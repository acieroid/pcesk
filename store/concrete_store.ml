open Address
open Storable
open Store

module Concrete_store : STORE =
  functor (Addr : ADDRESS) ->
  struct

    module StoreMap = Map.Make(Addr)

    type 'a t = 'a StoreMap.t


    let string_of_store store f =
      "env(" ^ (String.concat ","
                  (List.map (fun (a, v) ->
                    (Addr.string_of_address a) ^ ":" ^ (f v))
                     (StoreMap.bindings store))) ^ ")"

    let empty =
      StoreMap.empty

    let lookup store addr =
      StoreMap.find addr store

    let alloc store addr value =
      StoreMap.add addr value store

    let update = alloc

  end
