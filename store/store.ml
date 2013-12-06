open Address
open Lattice

(* The store binds addresses to (possibly multiple) abstract values,
   stored inside a lattice *)
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
       contained in the set of address.
       Also keep the non-reclaimable addresses *)
    val narrow : t -> Addr.t list -> t
    (* Convert a store to a string *)
    val string_of_store : t -> string
    (* Size of the store *)
    val size : t -> int
  end

(* Implementation of store using OCaml's Map *)
module Store : STORE =
  functor (Addr : ADDRESS) ->
  functor (Lattice : LATTICE) ->
  struct

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
          (not (Addr.is_reclaimable a)) || List.mem a addrs)
        store

    let size store =
      List.length (StoreMap.bindings store)

    let string_of_store store =
      "store(" ^ (String.concat ","
                    (List.map (fun (a, (v, _)) ->
                         (Addr.string_of_address a) ^ ":" ^
                           (Lattice.string_of_lattice_value v))
                        (StoreMap.bindings store))) ^ ")"

  end

(* Simple implementation of a store using association lists.
   Not fully correct (and thus not used) *)
module Assoc_store : STORE =
  functor (Addr : ADDRESS) ->
  functor (Lattice : LATTICE) ->
  struct

    type content = Lattice.t

    type t = (Addr.t * content) list

    let empty = []

    let lookup store addr =
      List.assoc addr store

    let alloc store addr value =
      (addr, value) :: (BatList.remove_assoc addr store)

    (* TODO: should join if necessary *)
    let update store addr value =
      let _ = List.assoc addr store in
      (addr, value) :: (BatList.remove_assoc addr store)

    let join store store' =
      let sort = List.sort (fun (a, _) (a', _) -> Addr.compare a a') in
      let rec loop acc = function
        | [], l | l, [] -> l @ acc
        | hd::tl, hd'::tl' ->
          match compare (fst hd) (fst hd') with
          | 0 -> loop (((fst hd), (Lattice.join (snd hd) (snd hd'))) :: acc)
                   (tl, tl')
          | n when n < 0 -> loop (hd :: acc) (tl, hd'::tl')
          | _ -> loop (hd' :: acc) (hd::tl, tl')
      in
      loop [] (sort store, sort store')

    let narrow store addrs =
      List.filter (fun (a, _) ->
          (not (Addr.is_reclaimable a)) ||
          (List.exists (fun a' -> Addr.compare a a' = 0) addrs))
        store

    let size store =
      List.length store

    let string_of_store store =
      "store(" ^ (String.concat ","
                    (List.map (fun (a, v) ->
                         (Addr.string_of_address a) ^ ":" ^
                           (Lattice.string_of_lattice_value v))
                        store))
  end
