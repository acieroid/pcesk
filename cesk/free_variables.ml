open Ast
open Util
open Primitives

let free_variables node =
  let rec free_variables' (exp, _) = match exp with
    | Identifier s -> StringSet.singleton s
    | String _ | Integer _ | Boolean _ | Nil -> StringSet.empty
    | Funcall (f, args) ->
      List.fold_left StringSet.union (free_variables' f)
        (List.map free_variables' args)
    | Lambda (vars, body) ->
      let free_in_body = List.fold_left StringSet.union StringSet.empty
          (List.map free_variables' body) in
      StringSet.diff free_in_body (string_set_of_vars vars)
    | Begin body ->
      List.fold_left StringSet.union StringSet.empty
        (List.map free_variables' body)
    | LetRec (bindings, body) ->
      let free_in_body = List.fold_left StringSet.union StringSet.empty
          (List.map free_variables' body)
      and free_in_bindings = List.fold_left StringSet.union StringSet.empty
          (List.map (fun (_, v) -> free_variables' v) bindings) in
      StringSet.diff (StringSet.union free_in_body free_in_bindings)
        (string_set_of_list (List.map (fun ((v, _), _) -> v) bindings))
    | If (cond, cons, alt) ->
      StringSet.union (free_variables' cond)
        (StringSet.union (free_variables' cons) (free_variables' alt))
    | Set ((name, _), value) ->
      StringSet.add name (free_variables' value)
    | Callcc n ->
      free_variables' n
    | Spawn n ->
      free_variables' n
    | Join n ->
      free_variables' n
    | Cas ((name, _), e1, e2) ->
      StringSet.add name
        (StringSet.union (free_variables' e1) (free_variables' e2)) in
  StringSet.diff (free_variables' node)
    (string_set_of_list (List.map fst primitives))
