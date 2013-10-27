open Address

module type ENV =
  functor (Addr : ADDRESS) ->
  sig
    type t
    (* The empty environment *)
    val empty : t
    (* Lookup a name in the environment and return the bound address.
       Raise a Not_found exception if the name is unbound *)
    val lookup : t -> string -> Addr.t
    (* Add a new binding to the environment (might shadow a previous one) *)
    val extend : t -> string -> Addr.t -> t
    (* Return a string representation of the environment *)
    val string_of_env : t -> string
  end

module Env : ENV =
  functor (Addr : ADDRESS) ->
  struct

    module EnvMap = Map.Make(String)

    type t = Addr.t EnvMap.t

    let empty =
      EnvMap.empty

    let lookup env name =
      EnvMap.find name env

    let extend env name address =
      EnvMap.add name address env

    let string_of_env env =
      "env(" ^ (String.concat ","
                  (List.map (fun (n, a) ->
                       n ^ ":" ^ (Addr.string_of_address a))
                      (EnvMap.bindings env))) ^ ")"

  end
