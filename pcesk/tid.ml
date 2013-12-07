open Types
open Pcesk_types
open Cesk_types

module type TID = sig
  val newtid : int -> context -> threads -> tid
end

module ConcreteTID = struct
  let newtid n _ _ = IntTid (n+1)
end

module ContextBoundTID =
  functor (Size : sig val size : int end) -> struct
    let newtid n _ _ = IntTid (n mod Size.size)
  end

module TimestampTid = struct
  let newtid _ context _ = match context.cexp with
    | Node (_, tag) -> TagTid (tag, context.ctime)
    | _ -> failwith "should not happen"
end
