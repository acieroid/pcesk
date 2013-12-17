open Pcesk_types
open Graph

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
  type t = pstate
  let compare = compare_pstates
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

let find_node graph id =
  G.fold_vertex (fun pstate -> function
    | Some state -> Some state
    | None when node_id pstate = id -> Some pstate
    | None -> None)
    graph None

module DotArg =
struct
  include G
  let edge_attributes ((_, e, _) : E.t) = [`Label (BatString.escaped e)]
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes (pstate : V.t) =
    [`Shape `Box; `Label (BatString.escaped
                            begin if !Params.verbose >= 2 then
                              (string_of_int (node_id pstate)) ^ " "
                            else
                              ""
                            end ^
                            (string_of_pstate ~color:false "" pstate))]
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
