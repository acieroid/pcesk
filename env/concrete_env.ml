open Address
open Env

module Concrete_env : ENV =
  functor (Address : ADDRESS) ->
  struct

    module EnvMap = Map.Make(String)

    type t = Address.t EnvMap.t

    let empty =
      EnvMap.empty

    let lookup env name =
      EnvMap.find name env

    let extend env name address =
      EnvMap.add name address env

  end
