open Util
open Graph
open Cesk_types
open Pcesk_types
open Types

let id = ref 0
let new_id () =
  id := !id + 1;
  !id

module PStateMap = Map.Make(PStateOrdered)

let nodes = ref PStateMap.empty

let node_id node =
  if PStateMap.mem node !nodes then
    PStateMap.find node !nodes
  else
    let id = new_id () in
    nodes := PStateMap.add node id !nodes;
    id

module GraphNode = struct
  type t = pstate
  let compare = compare_pstates
  let hash = Hashtbl.hash
  let equal = (=)
end

module GraphEdge = struct
  type t = tid * context
  let compare (t1, c1) (t2, c2) =
    order_concat [Pervasives.compare t1 t2;
                  compare_contexts c1 c2]
  let equal = (=)
  let default = (IntTid (-1), { cexp = Value (AbsUnique Nil);
                                cenv = Env.Env.empty;
                                caddr = TagAddr (-1, IntTime (-1));
                                cchange = Epsilon;
                                ctime = IntTime (-1) })
end

module G = Persistent.Digraph.ConcreteBidirectionalLabeled(GraphNode)(GraphEdge)

let find_node graph id =
  G.fold_vertex (fun pstate -> function
    | Some state -> Some state
    | None when node_id pstate = id -> Some pstate
    | None -> None)
    graph None

module GOper = Oper.P(G)

module DotArg =
struct
  include G
  let edge_attributes ((_, (tid, ctx), _) : E.t) = [`Label (string_of_tid tid)]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes (pstate : V.t) =
    [`Shape `Box; `Label (BatString.escaped
                            (begin if !Params.verbose >= 2 then
                                (string_of_int (node_id pstate)) ^ " "
                             else
                               ""
                            end
                             ^ (string_of_pstate ~color:false "" pstate)))]
  let vertex_name (pstate : V.t) =
    let state_id = (string_of_int (node_id pstate)) in
    "pstate_" ^ state_id
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end

module Dot = Graph.Graphviz.Dot(DotArg)

let output_graph graph file =
  let out = open_out_bin file in
  Dot.output_graph out graph;
  close_out out
