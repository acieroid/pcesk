open Types
open Cesk_types
open Exceptions

(** Environment *)

let empty_env = Env.empty
let env_lookup env name =
  try
    Env.lookup env name
  with
    Not_found -> raise (UnboundIdentifier name)
let env_extend env name a =
  (* print_string ("extend(" ^ name ^ ", " ^
                  (Addr.string_of_address a) ^ ")\n"); *)
  Env.extend env name a

(** Store *)

let print_store store =
  print_string ("store(" ^ (Store.string_of_store store) ^ ")\n")

let empty_store = Store.empty
let store_lookup store a =
  try
    Store.lookup store a
  with
    Not_found -> raise (UnboundAddress a)
let store_extend store a v =
  (* print_string ("store_extend(" ^ (Addr.string_of_address a) ^ ", " ^
                (Lattice.string_of_lattice_value v) ^ ")\n"); *)
  Store.alloc store a v
let store_update store a v =
  (* print_string ("store_update(" ^ (Addr.string_of_address a) ^ ", " ^
                  (Lattice.string_of_lattice_value v) ^ ")\n"); *)
  Store.update store a v

let extract_konts state =
  BatList.filter_map (function AbsUnique (Kont k) -> Some k | v -> None)
    (Lattice.conc (store_lookup state.store state.addr))

(** Allocation *)

let alloc (state : state) (tag : int) : addr = Addr.alloc tag

let alloc_prim (state : state) (name : string) : addr = Addr.alloc_prim name

let alloc_kont (state : state) = Addr.alloc_kont state.time

(** Time *)

let tick (state : state) : time = (state.time)+1

