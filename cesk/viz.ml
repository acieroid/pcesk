open Types
open Cesk_types

module GraphNode = struct
  type t = state
  let compare state state' =
    match (state.exp, state'.exp) with
    | Node _, Node _
    | Value _, Value _ -> Pervasives.compare state.time state'.time
    | Node _, Value _ -> 1
    | Value _, Node _ -> -1
  let hash state = Hashtbl.hash (state.exp, state.addr)
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
                 `Label (Scheme_ast.string_of_node n)]
    | Value v -> [`Shape `Box; `Style `Filled; `Fillcolor 0xDDFFDD;
                  `Label (string_of_value v)]
  let vertex_name (state : V.t) =
    match state.exp with
    | Node n -> "node_" ^ (string_of_int state.time)
    | Value v -> "value_" ^ (string_of_int state.time)
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end

module Dot = Graph.Graphviz.Dot(DotArg)

let output_graph file graph =
  let out = open_out_bin file in
  Dot.output_graph out graph;
  close_out out
