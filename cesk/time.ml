module type TIME = sig
  type t
  val compare : t -> t -> int
  val string_of_time : t -> string
  val initial : t
  val tick : t -> Ast.node -> t
end

module ConcreteTime : TIME = struct
  type t = int
  let compare = Pervasives.compare
  let string_of_time = string_of_int
  let initial = 0
  let tick t _ = t+1
end

module AbstractTime : TIME = struct
  type t = Ast.node list
  let compare = Pervasives.compare
  let string_of_time t =
    "[" ^ (String.concat ","
             (List.map Ast.string_of_node t)) ^ "]"
  let initial = []
  let tick t = function
    (* update the time only at a call site *)
    | ((Ast.Funcall _), _) as node ->
      BatList.take !Params.k (node :: t)
    | _ -> t
end
