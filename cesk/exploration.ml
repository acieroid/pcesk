(* The container used to do the exploration of the graph.
   Use in-place modification. *)
module type EXPLORATION =
sig
  type 'a t
  (* Create a new container containing the initial exploration state *)
  val create : 'a -> 'a t
  (* Check if the container is empty *)
  val is_empty : 'a t -> bool
  (* Add multiple values to the container. Modifies the container in-place. *)
  val add : 'a t -> 'a list -> unit
  (* Pick a value from the container. Modifies the container in place *)
  val pick : 'a t -> 'a
end

(* Depth-first search *)
module Dfs : EXPLORATION =
struct
  type 'a t = 'a Stack.t

  let create x =
    let stack = Stack.create () in
    Stack.push x stack;
    stack

  let is_empty = Stack.is_empty

  let rec add stack vs =
    List.iter (fun v -> Stack.push v stack) vs

  let pick stack =
    Stack.pop stack
end

(* Breadth-first search *)
module Bfs : EXPLORATION =
struct
  type 'a t = 'a Queue.t

  let create x =
    let queue = Queue.create () in
    Queue.add x queue;
    queue

  let is_empty = Queue.is_empty

  let rec add queue vs =
    List.iter (fun v -> Queue.add v queue) vs

  let pick queue =
    Queue.take queue
end
