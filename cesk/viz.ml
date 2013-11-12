open Types
open Cesk_types
open Cesk_base
open Graph


(* Hack to avoid merged states in dot output *)
let id = ref 0
let new_id () =
  id := !id + 1;
  !id

let nodes = Hashtbl.create 100

let node_id node =
  if Hashtbl.mem nodes node then
    Hashtbl.find nodes node
  else
    let id = new_id () in
    Hashtbl.add nodes node id;
    id

module GraphNode = struct
  type t = state
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module GraphEdge = struct
  type t = string
  let compare = Pervasives.compare
  let equal = (=)
  let default = ""
end

module G = Persistent.Digraph.ConcreteBidirectionalLabeled(GraphNode)(GraphEdge)

module DotArg =
struct
  include G
  let edge_attributes ((_, e, _) : E.t) = [`Label e]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes (state : V.t) =
    match state.exp with
    | Node n -> [`Shape `Box; `Style `Filled; `Fillcolor 0xFFDDDD;
                 `Label (BatString.escaped
                           (BatString.slice ~last:50 (Ast.string_of_node n)))]
    | Value v -> [`Shape `Box; `Style `Filled; `Fillcolor 0xDDFFDD;
                  `Label (BatString.escaped (string_of_value v))]
  let vertex_name (state : V.t) =
    let state_id = (string_of_int (node_id state)) in
    match state.exp with
    | Node n -> "node_" ^ state_id
    | Value v -> "value_" ^ state_id
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end

module Dot = Graph.Graphviz.Dot(DotArg)

let output_graph file graph =
  let out = open_out_bin file in
  Dot.output_graph out graph;
  close_out out
