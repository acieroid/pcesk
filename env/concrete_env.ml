open Address
open Env

module Concrete_env : ENV =
  functor (Addr : ADDRESS) ->
  struct

    module EnvMap = Map.Make(String)

    type t = Addr.t EnvMap.t

    let string_of_env env =
      "env(" ^ (String.concat ","
                  (List.map (fun (n, a) ->
                    n ^ ":" ^ (Addr.string_of_address a))
                     (EnvMap.bindings env))) ^ ")"

    let empty =
      EnvMap.empty

    let lookup env name =
      (* print_string ("lookup(" ^ name ^ "," ^ (string_of_env env) ^ ")\n"); *)
      EnvMap.find name env

    let extend env name address =
      EnvMap.add name address env

  end
