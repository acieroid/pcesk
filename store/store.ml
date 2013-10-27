open Address

module type STORE =
  functor (Addr : ADDRESS) ->
  sig
    type +'a t
    (* The empty store *)
    val empty : 'a t
    (* Lookup a value at an address in the store and return it.
       Raise a Not_found exception if it does not exists *)
    val lookup : 'a t -> Addr.t -> 'a
    (* Add a new value into the store *)
    val alloc : 'a t -> Addr.t -> 'a -> 'a t
    (* Update a value from the store *)
    val update : 'a t -> Addr.t -> 'a -> 'a t
    (* Join two stores, merging the elements stored at the same
       address *)
    val join : 'a t -> 'a t -> 'a t
    (* Narrow a store, keeping only the elements whose address is
       contained in the set of address *)
    val narrow : 'a t -> Addr.t list -> 'a t
    (* Convert a store to a string, converting values according
       to a given function *)
    val string_of_store : 'a t -> ('a -> string) -> string
  end

module Store : STORE =
  functor (Addr : ADDRESS) ->
  struct

    module StoreMap = Map.Make(Addr)

    type 'a t = 'a StoreMap.t

    let empty = StoreMap.empty

    let lookup store addr = StoreMap.find addr store

    let alloc store addr value = StoreMap.add addr value store

    (* TODO: lattice, merge *)
    let update = alloc

    let join s s' =
      List.fold_left (fun store (a, v) -> update store a v) s
        (StoreMap.bindings s')

    let narrow store addrs =
      List.fold_left (fun store (a, v) ->
          if List.mem a addrs then
            alloc store a v
          else
            store)
        empty (StoreMap.bindings store)


    let string_of_store store f =
      "env(" ^ (String.concat ","
                  (List.map (fun (a, v) ->
                       (Addr.string_of_address a) ^ ":" ^ (f v))
                      (StoreMap.bindings store))) ^ ")"

  end
