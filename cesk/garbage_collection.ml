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
  | AbsUnique (Kont k) -> live_location_kont store k
  | _ -> AddressSet.empty

and live_locations_node store env node =
  live_location_env (Env.restrict env (free_variables node))

and live_location_env env =
  List.fold_left (fun s a -> AddressSet.add a s) AddressSet.empty
    (Env.range env)

and live_location_kont store = function
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
