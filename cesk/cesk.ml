open Types
open Cesk_types
open Cesk_base
open Primitives
open Viz
open Garbage_collection

(** Helper functions *)

let tick state = match state.exp with
  | Node n -> Time.tick state.time n
  | _ -> state.time

let state_push_old state node store addr =
  { state with
    exp = Node node;
    addr = addr;
    store = store;
    change = Push;
    time = tick state }

let state_push state node kont =
  let a = alloc_kont state node in
  let store = store_extend1 state.store a (AbsUnique (Kont kont)) in
  { state with
    exp = Node node;
    addr = a;
    store = store;
    change = Push;
    time = tick state }

let state_produce_value state value =
  { state with
    exp = Value value;
    change = Pop;
    time = tick state}

(** Keywords *)

let step_lambda state tag args body =
  [state_produce_value state
     (AbsUnique (Closure ((args, body), state.env)))]

let step_begin state tag = function
  | [] ->
    [{ state with
       exp = Value (Aval.aval Unspecified);
       change = Epsilon;
       time = tick state }]
  | (_, tag) as node :: rest ->
    let kont = BeginKont (tag, rest, state.env, state.addr) in
    [state_push state node kont]

let rec letrec_bind state bindings = match bindings with
  | [] -> state
  | ((name, tag), _) :: rest ->
    let a = alloc state tag in
    let env = env_extend state.env name a in
    let store = store_extend state.store a Lattice.bottom in
    letrec_bind { state with env; store } rest

let step_letrec state tag bindings body = match bindings with
  | [] -> step_begin state tag body
  | ((name, tag), node) :: rest ->
    let a = env_lookup state.env name in
    let kont = LetRecKont (tag, a, rest, body, state.env, state.addr) in
     [state_push state node kont]

let step_if state tag cond cons alt =
  let kont = IfKont (tag, cons, alt, state.env, state.addr) in
  [state_push state cond kont]

let step_set state tag id value =
  let kont = SetKont (tag, id, state.env, state.addr) in
  [state_push state value kont]

let step_callcc state tag exp =
  let kont = CallccKont (tag, state.env, state.addr) in
  [state_push state exp kont]

(** State manipulation *)

let rec apply_function rator rands state = match rator with
  | AbsUnique (Closure ((ids, body), env)) ->
    if List.length ids != List.length rands then
      []
    else
      let args = List.combine ids rands in
      let addrs, state = List.fold_right
          (fun ((name, tag), value) (addrs, state) ->
             let a = alloc state tag in
             (name, value, a) :: addrs, {state with time = tick state})
          args ([], state) in
      let extended_env = List.fold_left
          (fun env (name, value, a) ->
             env_extend env name a) env addrs
      and extended_store = List.fold_left
          (fun store (name, value, a) ->
             store_extend1 store a value)
          state.store addrs in
      begin match body with
        | [] -> failwith "Should not happen"
        | [form] ->
          (* Only one form in body *)
          [{ state with
             exp = Node form;
             env = extended_env;
             store = extended_store;
             change = Pop;
             time = tick state}]
        | _ ->
          (* Multiple forms in body, implicit begin *)
          let (_, tag) = (List.hd body) in
          step_begin state tag body
      end
  | AbsUnique (Primitive prim) ->
    begin match apply_primitive prim rands with
      | Some v ->
        [{ state with
           exp = Value v;
           change = Pop;
           time = tick state}]
      | None -> []
    end
  | AbsUnique (Kont k) ->
    begin match rands with
      | [rand] -> step_value state rand  k
      | _ -> []
    end
  | _ -> []

and step_node state (e, tag) =
  match e with
  | Ast.Identifier x ->
    let values = Lattice.conc
        (store_lookup state.store (env_lookup state.env x)) in
    List.map (state_produce_value state) values
  | Ast.String s ->
    [state_produce_value state (Aval.aval (String s))]
  | Ast.Integer n ->
    [state_produce_value state (Aval.aval (Integer n))]
  | Ast.Boolean b ->
    [state_produce_value state (Aval.aval (Boolean b))]
  | Ast.Nil ->
    [state_produce_value state (Aval.aval Nil)]
  | Ast.Lambda (vars, body) ->
    step_lambda state tag vars body
  | Ast.Begin body ->
    step_begin state tag body
  | Ast.LetRec (bindings, body) ->
    step_letrec (letrec_bind state bindings) tag bindings body
  | Ast.If (cond, cons, alt) ->
    step_if state tag cond cons alt
  | Ast.Set ((var, tag), value) ->
    step_set state tag var value
  | Ast.Callcc (exp, tag) ->
    step_callcc state tag (exp, tag)
  | Ast.Funcall (((_, tag) as rator), rands) ->
    let kont = OperatorKont (tag, rands, state.env, state.addr) in
    [state_push state rator kont]
  | Ast.Cas ((x, _), e_old, e_new) ->
    let addr = env_lookup state.env x in
    let value = store_lookup state.store addr in
    let v_old = eval_atomic e_old state.env state.store
    and v_new = eval_atomic e_new state.env state.store in
    let state_eq = { (state_produce_value state (Aval.aval (Boolean true)))
                     with store = store_update state.store addr v_new }
    and state_neq = state_produce_value state (Aval.aval (Boolean false)) in
    let proj = Lattice.meet value v_old in
    if Lattice.is_bottom proj then
      (* x can't be equal to v_old *)
      [state_neq]
    else if value = v_old then
      (* TODO: this branch should be taken *only* when value is is a
         single value (eg. if value = AbsInt and v_old = AbsInt, they
         might still be different *)
      (* x is equal to v_old *)
      [state_eq]
    else
      (* x can be equal or not equal to v_old *)
      [state_eq; state_neq]
  | Ast.Spawn _
  | Ast.Join _ -> failwith "Can't deal with parallelism in CESK machine"

and step_value state v kont =
  match kont with
  (** Operator *)
  | OperatorKont (_, [], env, c) ->
    apply_function v [] { state with
                          env = env;
                          addr = c;
                          time = tick state }
  | OperatorKont (_, ((_, tag) as rand) :: rands, env, c) ->
    let kont = OperandsKont (tag, v, rands, [], env, c) in
    [state_push { state with env } rand kont]
  (** Operands *)
  | OperandsKont (_, rator, [], values, env, c) ->
    let rands = List.rev (v :: values) in
    apply_function rator rands { state with
                                 env = env;
                                 addr = c;
                                 time = tick state }
  | OperandsKont (_, rator, ((_, tag) as rand) :: rands, values, env, c) ->
    let kont = OperandsKont (tag, rator, rands, v :: values, env, c) in
    [state_push { state with env } rand kont]
  (** begin *)
  | BeginKont (_, [], env, c) ->
    [{ state with
       exp = Value v;
       env = env;
       addr = c;
       change = Epsilon;
       time = tick state }]
  | BeginKont (_, [node], env, c) ->
    [{ state with
       exp = Node node;
       env = env;
       addr = c;
       change = Epsilon;
       time = tick state }]
  | BeginKont (_, ((_, tag) as node) :: rest, env, c) ->
    let kont = BeginKont (tag, rest, env, c) in
    [state_push { state with env } node kont]
  (** letrec *)
  | LetRecKont (_, a, bindings, body, env, c) ->
    let store = store_update state.store a (Lattice.abst1 v) in
    begin match bindings with
      | [] ->
        begin match body with
          | [] -> failwith "letrec: empty body"
          | (_, tag) as node :: rest ->
            let kont = BeginKont (tag, rest, env, c) in
            [state_push { state with env; store } node kont]
        end
      | ((name, tag), node) :: rest ->
        let a = alloc state tag in
        let env = env_extend env name a in
        let store = store_extend store a Lattice.bottom  in
        let kont = LetRecKont (tag, a, rest, body, env, c) in
        [state_push { state with env; store } node kont]
    end
  (** if *)
  | IfKont (_, consequent, alternative, env, c) ->
    let new_state = { state with
                      env;
                      addr = c;
                      change = Pop;
                      time = tick state } in
    let state_true = { new_state with exp = Node consequent }
    and state_false = { new_state with exp = Node alternative }
    and l_false = Lattice.abst1 (AbsUnique (Boolean false))
    and l_v = Lattice.abst1 v in
    let proj = Lattice.meet l_v l_false in
    if Lattice.is_bottom proj then
      (* v can't be false *)
      [state_true]
    else if l_v = l_false then
      (* v is false *)
      [state_false]
    else
      (* either true or false *)
      [state_true; state_false]
  (** set! *)
  | SetKont (_, id, env, c) ->
    let a = env_lookup env id in
    let store = store_update state.store a (Lattice.abst1 v) in
    [{ state with
       exp = Value (Aval.aval Unspecified);
       store = store;
       addr = c;
       change = Epsilon;
       time = tick state }]
  (** call/cc *)
  | CallccKont (tag, env, c) ->
    let state' =  { state with
                    env = env;
                    addr = c;
                    time = tick state } in
    let ks = extract_konts state' in
    List.concat (List.map (fun k ->
        apply_function v [AbsUnique (Kont k)] state')
        ks)
  (** Halt *)
  | HaltKont -> [{ state with change = Epsilon; time = tick state}]

let step state =
  let state = if !Params.gc && not !Params.parallel then gc state else state in
  match state.exp with
  | Node n -> step_node state n
  | Value v -> List.flatten (List.map (step_value state v)
                               (extract_konts state))

(** Injection *)

let empty_address = TagAddr (0, Time.initial)

let empty_state = {
  exp = Value (Aval.aval (Integer 0));
  env = empty_env;
  store = empty_store;
  addr = empty_address;
  change = Epsilon;
  time = Time.initial;
}

let inject e =
  let state = install_primitives empty_state in
  let a_halt = alloc_kont state e in
  let store = store_extend1 state.store a_halt (AbsUnique (Kont HaltKont)) in
  ({ state with
     exp = Node e;
     store = store;
     addr = a_halt;
     time = tick state},
   a_halt)

(** Evaluation *)

let string_of_konts konts =
  String.concat "|" (List.map string_of_kont konts)

let string_of_state state =
  (match state.exp with
   | Node n -> "\027[31m" ^ (Ast.string_of_node n) ^ "\027[0m"
   | Value v -> "\027[32m" ^ (string_of_value v) ^ "\027[0m")

let string_of_update state state' =
  let str pop = match state'.change with
    | Epsilon -> "e"
    | Pop -> pop
    | Push -> "+" ^ (string_of_konts (extract_konts state')) in
  (* Some special cases to have it working correctly.
     Could probably be made in a cleaner way *)
  match state.exp, state'.exp with
  | Node _, _ -> str ("-" ^ (string_of_konts (extract_konts state)))
  | Value _, Value _ -> str ("-" ^ (string_of_konts (extract_konts state')))
  | Value _, Node _ -> str "e"

module StateSet = Set.Make(StateOrdered)

let eval e =
  let module Exploration = (val !Params.exploration) in
  let (initial_state, a_halt) = inject e in
  let extract_final state =
    match state.exp, state.addr with
    | Value result, addr when addr = a_halt ->
      Some (result, state.env, state.store)
    | _ -> None
  and todo = Exploration.create initial_state in
  let rec loop visited finished graph =
    if Exploration.is_empty todo then
      finished, graph
    else
      let state = Exploration.pick todo in
      let found = StateSet.mem state visited in
      if found then
        loop visited finished graph
      else match extract_final state with
        | Some res ->
          loop (StateSet.add state visited) (res::finished) graph
        | None ->
          let states = List.map (fun state ->
              if !Params.gc_after then gc state else state)
              (step state) in
          let source = G.V.create state
          and dests = List.map G.V.create states in
          let edges = List.map (fun dest -> G.E.create source
                                   (string_of_update source dest)
                                   dest) dests in
          let graph' =
            List.fold_left G.add_edge_e
              (List.fold_left G.add_vertex graph dests) edges in
          if !Params.verbose >= 1 then begin
            print_string ("==> " ^ (string_of_state state) ^ "\n");
            List.iter (fun state' ->
                print_string ("    " ^
                                (string_of_update state state') ^ " -> " ^
                                (string_of_state state') ^ "\n")) states;
            print_newline ();
          end;
          Exploration.add todo states;
          loop (StateSet.add state visited) finished graph'
  in
  let initial_graph = G.add_vertex G.empty (G.V.create initial_state) in
  loop StateSet.empty [] initial_graph
