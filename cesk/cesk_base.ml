open Types
open Env
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

let alloc state tag = TagAddr (tag, state.time)

let alloc_var state name = VarAddr (name, state.time)

let alloc_kont state node = KontAddr (node, state.time)

(** Time *)

let tick (state : state) : time = match state.exp with
  | Node node ->
    begin match state.time with
      | Some n -> Some node
      | None ->
        (* 0-CFA *)
        None
        (* 1-CFA *)
        (* Some node *)
    end
  | Value _ -> state.time
