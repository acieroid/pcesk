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
  (* debug ["extend("; name; ", "; Addr.string_of_address a; ")"]; *)
  Env.extend env name a

(** Store *)

let print_store store =
  print_string (Store.string_of_store store)

let empty_store = Store.empty

let store_lookup store a =
  try
    let v = Store.lookup store a in
    (* debug ["store_lookup("; Addr.string_of_address a; ") -> ";
           Lattice.string_of_lattice_value v; ")"]; *)
    v
  with
    Not_found -> raise (UnboundAddress a)

let store_extend store a v =
  (* debug ["store_extend("; Addr.string_of_address a; ", ";
         Lattice.string_of_lattice_value v; ")"]; *)
  Store.alloc store a v

let store_update store a v =
  (* debug ["store_update("; Addr.string_of_address a; ", ";
         Lattice.string_of_lattice_value v; ")"]; *)
  Store.update store a v

let store_extend1 store a v =
  store_extend store a (Lattice.abst1 v)

let extract_konts state =
  BatList.filter_map (function AbsUnique (Kont k) -> Some k | v -> None)
    (Lattice.conc (store_lookup state.store state.addr))

(** Allocation *)

let alloc state tag = TagAddr (tag, state.time)

let alloc_var state name = VarAddr (name, state.time)

let alloc_prim state name = PrimAddr (name, state.time)

let alloc_kont state node = KontAddr (node, state.time)

(** Atomic evaluator *)
let eval_atomic node env store =
  let ret v = Lattice.abst1 (AbsUnique v) in
  match fst node with
  | Ast.Identifier v -> store_lookup store (env_lookup env v)
  | Ast.String s -> ret (String s)
  | Ast.Integer n -> ret (Integer n)
  | Ast.Boolean b -> ret (Boolean b)
  | Ast.Lambda (vars, body) -> ret (Closure ((vars, body), env))
  | _ -> raise (NotAtomic node)
