open Util
open Types
open Cesk_types
open Cesk_base
open Env
open Free_variables

let union = function
  | [] -> AddressSet.empty
  | hd :: tl -> List.fold_left AddressSet.union hd tl

let rec live_locations store exp env = match exp with
  | Value v -> live_locations_value store v
  | Node n -> live_locations_node store env n

and live_locations_value store = function
  | AbsUnique (Kont k) -> live_locations_kont store k
  | AbsUnique (Closure ((args, body), env)) ->
    live_locations_env
      (Env.restrict env
         (free_variables ((Ast.Lambda (args, body), 0))))
  | _ -> AddressSet.empty

and live_locations_node store env node =
  live_locations_env (Env.restrict env (free_variables node))

and live_locations_env env =
  List.fold_left (fun s a -> AddressSet.add a s) AddressSet.empty
    (Env.range env)

and live_locations_kont store = function
  | HaltKont -> AddressSet.empty
  | OperatorKont (_, args, env, addr) ->
    AddressSet.add addr
      (union
         [union (List.map (live_locations_node store env) args);
          live_locations_store store addr])
  | OperandsKont (_, op, nodes, values, env, addr) ->
    AddressSet.add addr
      (union
         [union (List.map (live_locations_value store) values);
          union (List.map (live_locations_node store env) nodes);
          live_locations_value store op;
          live_locations_store store addr])
  | BeginKont (_, body, env, addr) ->
    AddressSet.add addr
      (union
         [union (List.map (live_locations_node store env) body);
          live_locations_store store addr])
  | LetRecKont (_, _, bindings, body, env, addr) ->
    AddressSet.add addr
      (union
         [union (List.map (live_locations_node store env) body);
          union (List.map (fun (_, n) -> live_locations_node store env n)
                   bindings);
          live_locations_store store addr])
  | IfKont (_, cons, alt, env, addr) ->
    AddressSet.add addr
      (union
         [live_locations_node store env cons;
          live_locations_node store env alt;
          live_locations_store store addr])
  | SetKont (_, _, env, addr) ->
    AddressSet.add addr (live_locations_store store addr)

and live_locations_store store addr =
  let vs = Lattice.conc (store_lookup store addr) in
  union (List.map (live_locations_value store) vs)

let gc state =
  let rec reachable grey black =
    if AddressSet.is_empty grey then
      black
    else
      let addr = AddressSet.choose grey in
      let black' = AddressSet.add addr black in
      let grey' = AddressSet.diff
          (union [grey;
                  live_locations_store state.store addr])
          black' in
      reachable grey' black' in
  let grey = union [live_locations state.store state.exp state.env;
                    live_locations_store state.store state.addr]
  and black = AddressSet.singleton state.addr in
  let locations = reachable grey black in
  let store = Store.narrow state.store (AddressSet.elements locations) in
  { state with store = store }
