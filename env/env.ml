(* An environment binds names to some values (in our case, addresses) *)
module type ENV =
  sig
    type +'a t
    (* The empty environment *)
    val empty : 'a t
    (* Lookup a name in the environment and return the bound address.
       Raise a Not_found exception if the name is unbound *)
    val lookup : 'a t -> string -> 'a
    (* Add a new binding to the environment (might shadow a previous one) *)
    val extend : 'a t -> string -> 'a -> 'a t
    (* Return a string representation of the environment *)
    val string_of_env : ?print:('a -> string) -> 'a t -> string
  end

module Env : ENV =
  struct

    module EnvMap = Map.Make(String)

    type 'a t = 'a EnvMap.t

    let empty =
      EnvMap.empty

    let lookup env name =
      EnvMap.find name env

    let extend env name address =
      EnvMap.add name address env

    let string_of_env ?print env = match print with
      | Some f ->
        "env(" ^ (String.concat ","
                    (List.map (fun (n, a) ->
                       n ^ ":" ^ (f a))
                      (EnvMap.bindings env))) ^ ")"
      | None ->
        "env(" ^ (String.concat ","
                    (List.map fst (EnvMap.bindings env))) ^ ")"
  end
