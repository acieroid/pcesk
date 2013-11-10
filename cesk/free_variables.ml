open Scheme_ast
open Util
open Primitives

let free_variables node =
  let rec free_variables' (exp, _) = match exp with
    | Identifier s -> StringSet.singleton s
    | String _ | Integer _ | Boolean _ -> StringSet.empty
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
    | Define ((name, _), value) ->
      StringSet.diff (free_variables' value) (StringSet.singleton name)
    | DefineFun ((name, _), args, body) ->
      StringSet.diff (free_variables' ((Lambda (args, body), 0)))
        (StringSet.singleton name)
    | If (cond, cons, alt) ->
      StringSet.union (free_variables' cond)
        (StringSet.union (free_variables' cons) (free_variables' alt))
    | Set ((name, _), value) ->
      StringSet.add name (free_variables' value) in
  StringSet.diff (free_variables' node)
    (string_set_of_list (List.map fst primitives))
