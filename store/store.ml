open Address
open Lattice

module type STORE =
  functor (Addr : ADDRESS) ->
  functor (Lattice : LATTICE) ->
  sig
    type content = Lattice.t
    type t
    (* The empty store *)
    val empty : t
    (* Lookup a value at an address in the store and return it.
       Raise a Not_found exception if it does not exists *)
    val lookup : t -> Addr.t -> content
    (* Add a new value into the store *)
    val alloc : t -> Addr.t -> content -> t
    (* Update a value from the store. Raise Not_found if nothing
       exists at the given address *)
    val update : t -> Addr.t -> content -> t
    (* Join two stores, merging the elements stored at the same
       address *)
    val join : t -> t -> t
    (* Narrow a store, keeping only the elements whose address is
       contained in the set of address *)
    val narrow : t -> Addr.t list -> t
    (* Convert a store to a string *)
    val string_of_store : t -> string
  end

module Store : STORE =
  functor (Addr : ADDRESS) ->
  functor (Lattice : LATTICE) ->
  struct

    type content = Lattice.t
    module StoreMap = Map.Make(Addr)

    type freshness =
      | Empty
      | Fresh
      | NotFresh

    type t = (content * freshness) StoreMap.t

    let empty = StoreMap.empty

    let lookup store addr =
      let (v, _) = StoreMap.find addr store in
      v

    let alloc store addr value =
      let to_store =
        try
          let (v, f) = StoreMap.find addr store in
          match f with
          | Empty -> (value, Fresh)
          | _ -> (Lattice.join v value, NotFresh)
        with
          Not_found -> (value, Fresh)
      in
      StoreMap.add addr to_store store

    let update store addr value =
      let (v, f) = StoreMap.find addr store in
      let to_store = match f with
        | Empty -> raise Not_found
        | Fresh -> (value, Fresh)
        | NotFresh -> (Lattice.join v value, NotFresh) in
      StoreMap.add addr to_store store

    let join s s' =
      StoreMap.merge (fun a x y -> match x, y with
      | None, None -> None
      | Some v, None | None, Some v -> Some v
      | Some (v1, _), Some (v2, _) -> Some (Lattice.join v1 v2, NotFresh)) s s'

    let narrow store addrs =
      StoreMap.filter (fun a _ -> List.mem a addrs) store

    let string_of_store store =
      "env(" ^ (String.concat ","
                  (List.map (fun (a, (v, _)) ->
                       (Addr.string_of_address a) ^ ":" ^
                         (Lattice.string_of_lattice_value v))
                      (StoreMap.bindings store))) ^ ")"

  end
