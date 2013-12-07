open Types
open Pcesk_types

module type TID = sig
  val newtid : int -> context -> threads -> tid
end

module ConcreteTID = struct
  let newtid n _ _ = IntTid (n+1)
end
