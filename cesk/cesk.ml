open Types
open Cesk_types
open Cesk_base
open Exceptions
open Primitives
open Viz

(** Helper functions *)

let state_push_old state node store addr =
  { state with
    exp = Node node;
    addr = addr;
    store = store;
    change = Push;
    time = tick state }

let state_push state node kont =
  let a = alloc_kont state node in
  let store = store_extend state.store a (Lattice.abst1 (AbsUnique (Kont kont))) in
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

let step_lambda node state = function
  | args_node :: body ->
    begin match args_node with
      | (Scheme_ast.List args_node, tag) ->
        let args = List.map (function
            | (Scheme_ast.Identifier x, tag) -> (x, tag)
            | _ -> raise (Malformed ("lambda argument list", node))) args_node in
        begin match body with
          | [] -> raise (Malformed ("lambda without body", node))
          | body ->
            [state_produce_value state (AbsUnique (Closure ((args, body), state.env)))]
        end
      | _ -> raise NotYetImplemented
    end
  | _ -> raise (Malformed ("lambda without arguments", node))

let step_begin node state = function
  | [] ->
    [{ state with
       exp = Value (AbsUnique Unspecified);
       change = Epsilon;
       time = tick state }]
  | (_, tag) as n :: rest ->
    let kont = BeginKont (tag, rest, state.env, state.addr) in
    [state_push state n kont]

let step_define node state = function
  | [] -> raise (Malformed ("define without arguments", node))
  | [Scheme_ast.Identifier name, tag] ->
    let a = alloc state tag in
    let env' = env_extend state.env name a in
    let unsp = AbsUnique Unspecified in
    let store = store_extend state.store a (Lattice.abst1 unsp) in
    [{ state with
       exp = Value unsp;
       env = env';
       store = store;
       change = Epsilon;
       time = tick state }]
  | (Scheme_ast.Identifier name, tag) :: value :: [] ->
    let kont = DefineKont (tag, name, state.env, state.addr) in
    [state_push state value kont]
  | (Scheme_ast.List ((Scheme_ast.Identifier name, tag) :: args), _) :: body ->
    raise NotYetImplemented
  | _ -> raise (Malformed ("define with too much arguments", node))

let step_if node state = function
  | cond :: consequent :: [] ->
    raise NotYetImplemented
  | (_, tag) as cond :: consequent :: alternative :: [] ->
    let kont = IfKont (tag, consequent, alternative, state.env, state.addr) in
    [state_push state cond kont]
  | _ -> raise (Malformed ("if with an invalid number of arguments", node))

let step_set node state = function
  | (Scheme_ast.Identifier id, tag) :: value :: [] ->
    let kont = SetKont (tag, id, state.env, state.addr) in
    [state_push state value kont]
  | _ -> raise (Malformed ("set! with invalid arguments", node))

let keywords = [("lambda", step_lambda);
                ("begin", step_begin);
                ("define", step_define);
                ("if", step_if);
                ("set!", step_set)]

let is_keyword kw = List.mem_assoc kw keywords

let step_keyword (kw : string) (node : node) (args : node list)
    (state : state) : state list =
  try
    let f = List.assoc kw keywords in
    f node state args
  with
    Not_found -> raise (InvalidKeyword kw)

(** State manipulation *)

let apply_function (rator : value) (rands : value list)
    (state : state) : state list = match rator with
  | AbsUnique (Closure ((ids, body), env)) ->
    if List.length ids != List.length rands then
      raise (InvalidNumberOfArguments (List.length ids, List.length rands));
    let args = List.combine ids rands in
    let addrs, state = List.fold_right (fun ((name, tag), value) (addrs, state) ->
        let a = alloc state tag in
        (name, value, a) :: addrs, {state with time = tick state})
        args ([], state) in
    let extended_env = List.fold_left
        (fun env (name, value, a) ->
           env_extend env name a) state.env addrs
    and extended_store = List.fold_left
        (fun store (name, value, a) ->
           store_extend store a (Lattice.abst1 value)) state.store addrs in
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
        (* TODO: should pass the node corresponding to the lambda *)
        step_keyword "begin" (List.hd body) body state
    end
  | AbsUnique (Primitive prim) ->
    begin match apply_primitive prim rands with
    | Some v ->
      [{ state with
         exp = Value v;
         change = Epsilon;
         time = tick state}]
    | None -> []
    end
  | _ -> []

let step (state : state) : state list =
  let step_node (e, tag) =
    match e with
    | Scheme_ast.Identifier x ->
      let values = Lattice.conc
          (store_lookup state.store (env_lookup state.env x)) in
      List.map (state_produce_value state) values
    | Scheme_ast.String s ->
      [state_produce_value state (AbsUnique (String s))]
    | Scheme_ast.Integer n ->
      [state_produce_value state (AbsUnique (Integer n))]
    | Scheme_ast.Boolean b ->
      [state_produce_value state (AbsUnique (Boolean b))]
    | Scheme_ast.List ((Scheme_ast.Identifier kw, tag') :: args)
      when is_keyword kw ->
      step_keyword kw (e, tag) args state
    | Scheme_ast.List ((_, tag) as rator :: rands) ->
      let kont = OperatorKont (tag, rands, state.env, state.addr) in
      [state_push state rator kont]
    | _ -> raise (EvaluationStuck (e, tag))
  and step_value v kont =
    match kont with
    | OperatorKont (_, [], env, c) ->
      apply_function v [] { state with
                            env = env;
                            addr = c;
                            time = tick state }
    | OperatorKont (_, ((_, tag) as rand) :: rands, env, c) ->
      let kont = OperandsKont (tag, v, rands, [], env, c) in
      [{(state_push state rand kont) with env = env}]
    | OperandsKont (_, rator, [], values, env, c) ->
      let rands = List.rev (v :: values) in
      apply_function rator rands { state with
                                   env = env;
                                   addr = c;
                                   time = tick state }
    | OperandsKont (_, rator, ((_, tag) as rand) :: rands, values, env, c) ->
      let kont = OperandsKont (tag, rator, rands, v :: values, env, c) in
      [{(state_push state rand kont) with env = env }]
    | BeginKont (_, [], _, c) ->
      [{ state with
         exp = Value v;
         addr = c;
         change = Epsilon;
         time = tick state }]
    | BeginKont (_, ((_, tag) as node) :: rest, env, c) ->
      let kont = BeginKont (tag, rest, env, c) in
      [state_push state node kont]
    | DefineKont (tag, name, env, c) ->
      let a = alloc state tag in
      let env' = env_extend env name a in
      let store = store_extend state.store a (Lattice.abst1 v) in
      [{ exp = Value (AbsUnique Unspecified);
         env = env';
         store = store;
         addr = c;
         change = Epsilon;
         time = tick state }]
    | IfKont (_, consequent, alternative, env, c) ->
      let new_state = { state with
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
    | SetKont (_, id, env, c) ->
      let a = env_lookup env id in
      let store = store_update state.store a (Lattice.abst1 v) in
      [{ state with
         exp = Value (AbsUnique Unspecified);
         store = store;
         addr = c;
         change = Epsilon;
         time = tick state }]
    | HaltKont -> [{ state with change = Epsilon; time = tick state }]
  in
  match state.exp with
  | Node n -> step_node n
  | Value v -> List.flatten (List.map (step_value v) (extract_konts state))

(** Injection *)

let empty_time = []

let empty_address = TagAddr (0, empty_time)

let empty_state = {
  exp = Value (AbsUnique (Integer 0));
  env = empty_env;
  store = empty_store;
  addr = empty_address;
  change = Epsilon;
  time = empty_time;
}

let inject (e : node) : state * addr =
  let state = install_primitives empty_state in
  let a_halt = alloc_kont state e in
  let store = store_extend state.store a_halt (Lattice.abst1 (AbsUnique (Kont HaltKont))) in
  ({ state with
     exp = Node e;
     store = store;
     addr = a_halt;
     time = tick state},
   a_halt)

(** Evaluation *)

let string_of_konts konts =
  String.concat "|" (List.map string_of_kont konts)

let string_of_state (state : state) : string =
  (match state.exp with
   | Node n -> (string_of_int (Hashtbl.hash state)) ^ "@node \027[31m" ^ (Scheme_ast.string_of_node n) ^ "\027[0m"
   | Value v -> (string_of_int (Hashtbl.hash state)) ^ "@value \027[32m" ^ (string_of_value v) ^ "\027[0m")

let string_of_update state state' = match state'.change with
  | Epsilon -> "ε"
  | Pop -> "-" ^ (string_of_konts (extract_konts state))
  | Push -> "+" ^ (string_of_konts (extract_konts state'))

module StateSet = Set.Make(struct
    type t = state
    let compare = Pervasives.compare
  end)

let eval ?(verbose = false) (e : node) : (value * env * store) list * G.t =
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
      try
        let _ = StateSet.find state visited in
        loop visited finished graph
      with
        Not_found ->
        begin match extract_final state with
          | Some res ->
            loop (StateSet.add state visited) (res::finished) graph
          | None ->
            let states = step state in
            let source = G.V.create state
            and dests = List.map G.V.create states in
            let edges = List.map (fun dest -> G.E.create source
                                     (string_of_update source dest)
                                     dest) dests in
            let graph' =
              List.fold_left G.add_edge_e
                (List.fold_left G.add_vertex graph dests) edges in
            if verbose then begin
              print_string ("==> " ^ (string_of_state state) ^ "\n");
              List.iter (fun state' ->
                  print_string ("    " ^
                                  (string_of_update state state') ^ " -> " ^
                                  (string_of_state state') ^ "\n")) states;
              print_newline ();
            end;
            Exploration.add todo states;
            loop (StateSet.add state visited) finished graph'
        end
  in
  let initial_graph = G.add_vertex G.empty (G.V.create initial_state) in
  loop StateSet.empty [] initial_graph
