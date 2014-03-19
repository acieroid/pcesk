open Address
open Lattice

(* The store binds addresses to (possibly multiple) abstract values, stored
 * inside a lattice *)
module type STORE =
  functor (Addr : ADDRESS) ->
  functor (Lattice : LATTICE) -> sig
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
       contained in the set of address.
       Also keep the non-reclaimable addresses *)
    val narrow : t -> Addr.t list -> t

    (* Compare two stores (Pervasives.compare might produce wrong results for
       store wit the same content, but a different internal structure) *)
    val compare : t -> t -> int

    (* Check if the first store subsumes the second one (ie. anything contained
       in the second store is also contained in the first one) *)
    val subsumes : t -> t -> bool

    (* Size of the store *)
    val size : t -> int

    (* Convert a store to a string *)
    val string_of_store : t -> string
  end

(* Implementation of store using OCaml's Map *)
module Store : STORE =
  functor (Addr : ADDRESS) ->
  functor (Lattice : LATTICE) -> struct

    type content = Lattice.t
    module StoreMap = Map.Make(Addr)

    type freshness =
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
          (Lattice.join v value, NotFresh)
        with
          Not_found -> (value,
                        if !Params.store_strong_updates then
                          Fresh
                        else
                          NotFresh)
      in
      StoreMap.add addr to_store store

    let update store addr value =
      let (v, f) = StoreMap.find addr store in
      let to_store = match f with
        | Fresh -> (value, Fresh)
        | NotFresh -> (Lattice.join v value, NotFresh) in
      StoreMap.add addr to_store store

    let join s s' =
      StoreMap.merge (fun a x y -> match x, y with
          | None, None -> None
          | Some v, None | None, Some v -> Some v
          | Some (v1, _), Some (v2, _) ->
            Some (Lattice.join v1 v2, NotFresh)) s s'

    let narrow store addrs =
      StoreMap.filter (fun a _ ->
          (if (not (Addr.is_reclaimable a)) || List.mem a addrs then
             true
           else begin
             Util.debug ["reclaim("; (Addr.string_of_address a); ")\n"];
             false
           end))
        store

    let compare s1 s2 =
      StoreMap.compare Pervasives.compare s1 s2

    let subsumes s1 s2 =
      (* TODO: a way to improve performances of this function would be todefine
       * our own map whose 'compare' function would be very similarto
       * Map.S.compare, but would have the properties we want when a key exists
       * in only one of the two maps *)
      let merge_val _ v1 v2 =
        (* TODO: what about freshness? *)
        match v1, v2 with
        | Some (x, _), Some (y, _) when x = y -> None
        | Some (x, _), Some (y, _) when Lattice.meet x y = x -> None
        | Some _, Some _ -> v2
        | None, Some _ -> v2
        | Some _, None -> None
        | None, None -> None in
      (* Merge the two store by removing keys that either have equal values, or
       * whose value in s1 subsumes value in s2. If the store resulting fromthis
       * merge is empty, it means that all the values in s2 are subsumedin s2.
       *)
      StoreMap.is_empty (StoreMap.merge merge_val s1 s2)

    let size store =
      List.length (StoreMap.bindings store)

    let string_of_store store =
      "store(" ^ (String.concat ","
                    (BatList.filter_map (fun (a, (v, _)) ->
                         if Addr.is_reclaimable a then
                           match Lattice.conc v with
                           | [Types.AbsUnique (Types.Kont _)] -> None
                           | _ ->
                             Some ((Addr.string_of_address a) ^ ":" ^
                                   (Lattice.string_of_lattice_value v))
                         else
                           None)
                        (StoreMap.bindings store))) ^ ")"

  end
