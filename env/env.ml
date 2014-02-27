open Util

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

    (* Restrict the environment to the names in the given set *)
    val restrict : 'a t -> StringSet.t -> 'a t

    (* Return the range of the environment (ie. the list of values
        contained in it *)
    val range : 'a t -> 'a list

    (* Compare two environments *)
    val compare : 'a t -> 'a t -> int

    (* Check if the first environment subsumes the second one *)
    val subsumes : 'a t -> 'a t -> bool

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

    let restrict env names =
      EnvMap.filter (fun name _ -> StringSet.mem name names) env

    let range env =
      List.map snd (EnvMap.bindings env)

    let compare e1 e2 =
      EnvMap.compare Pervasives.compare e1 e2

    let subsumes e1 e2 =
      (* TODO: see store's subsumes *)
      let merge_val _ v1 v2 =
        match v1, v2 with
        | Some x, Some y when x = y -> None
        | Some _, Some _ -> v2
        | None, Some _ -> v2
        | Some _, None -> None
        | None, None -> None in
      EnvMap.is_empty (EnvMap.merge merge_val e1 e2)

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
