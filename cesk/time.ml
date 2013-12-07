open Types

module type TIME = sig
  val compare : time -> time -> int
  val initial : time
  val tick : time -> Ast.node -> time
end

module ConcreteTime : TIME = struct
  let compare = Pervasives.compare
  let initial = IntTime 0
  let tick t _ = match t with
    | IntTime n -> IntTime (n+1)
    | _ -> failwith "invalid time"
end

module AbstractTime : TIME = struct
  let compare = Pervasives.compare
  let initial = KCallSitesTime []
  let tick t = function
    (* update the time only at a call site *)
    | ((Ast.Funcall _), _) as node ->
      begin match t with
      | KCallSitesTime l ->
        KCallSitesTime (BatList.take !Params.k (node :: l))
      | _ -> failwith "invalid time"
      end
    | _ -> t
end
