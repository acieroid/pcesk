open Types
open Cesk_types
open Cesk_base

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

module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(GraphNode)(GraphEdge)

module DotArg =
struct
  include G
  let edge_attributes ((a, e, b) : E.t) = [`Label e]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes (state : V.t) =
    match state.exp with
    | Node n -> [`Shape `Box; `Style `Filled; `Fillcolor 0xFFDDDD;
                 `Label (BatString.escaped (Scheme_ast.string_of_node n))]
    | Value v -> [`Shape `Box; `Style `Filled; `Fillcolor 0xDDFFDD;
                  `Label (BatString.escaped (string_of_value v))]
  let vertex_name (state : V.t) =
    let konts = extract_konts state in
    match state.exp with
    | Node n -> "node_" ^ (string_of_int (Hashtbl.hash state)) ^ (string_of_int (Hashtbl.hash konts))
    | Value v -> "value_" ^ (string_of_int (Hashtbl.hash state)) ^ (string_of_int (Hashtbl.hash konts))
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end

module Dot = Graph.Graphviz.Dot(DotArg)

let output_graph file graph =
  let out = open_out_bin file in
  Dot.output_graph out graph;
  close_out out
