type tag = int
type var = string * tag
type exp =
  | Identifier of string
  | String of string
  | Integer of int
  | Boolean of bool
  | Nil
  | Funcall of node * node list
  | Lambda of var list * node list
  | Begin of node list
  | LetRec of (var * node) list * node list
  | If of node * node * node
  | Set of var * node
  | Callcc of node
  | Spawn of node
  | Join of node (* argument of join should be an atomic expression *)
  | Cas of var * node * node (* both node arguments should be atomic *)
  | Locked
  | Unlocked
  | Acquire of var
  | Release of var
and node = exp * int

let rec string_of_exp ?tags:(tags=false) = function
  | Identifier s -> s
  | String s -> "\"" ^ s ^ "\""
  | Integer n -> string_of_int n
  | Boolean true -> "#t"
  | Boolean false -> "#f"
  | Nil -> "nil"
  | Funcall (f, args) ->
    "(" ^ (string_of_node ~tags f) ^ " " ^ (string_of_nodes ~tags " " args) ^
    ")"
  | Lambda (args, body) ->
    "(lambda (" ^ (string_of_vars ~tags " " args) ^ ") " ^
    (string_of_nodes ~tags " " body) ^ ")"
  | Begin [] -> "(begin)"
  | Begin body ->
    "(begin " ^ (string_of_nodes ~tags " " body) ^ ")"
  | LetRec (bindings, body) ->
    "(letrec (" ^ (string_of_bindings ~tags bindings) ^ ") " ^
    (string_of_nodes ~tags " " body) ^ ")"
  | If (cond, cons, alt) ->
    "(if " ^ (string_of_node ~tags cond) ^ " " ^
    (string_of_node ~tags cons) ^ " " ^
    (string_of_node ~tags alt) ^ ")"
  | Set ((v, _), e) ->
    "(set! " ^ v ^ " " ^ (string_of_node ~tags e) ^ ")"
  | Callcc e ->
    "(callcc " ^ (string_of_node ~tags e) ^ ")"
  | Spawn e ->
    "(spawn " ^ (string_of_node ~tags e) ^ ")"
  | Join e ->
    "(join " ^ (string_of_node ~tags e) ^ ")"
  | Cas ((v, _), e1, e2) ->
    "(cas " ^ v ^ " " ^ (string_of_node ~tags e1) ^ " " ^
    (string_of_node ~tags e2) ^ ")"
  | Locked -> "#locked"
  | Unlocked -> "#unlocked"
  | Acquire (v, _) ->
    "(acquire " ^ v ^ ")"
  | Release (v, _) ->
    "(release " ^ v ^ ")"

and string_of_node ?tags:(tags=false) (exp, tag) =
  string_of_exp ~tags exp ^ (if tags then "@" ^ (string_of_int tag) else "")

and string_of_nodes ?tags:(tags=false) sep nodes =
  String.concat sep (List.map (string_of_node ~tags) nodes)

and string_of_vars ?tags:(tags=false) sep vars =
  String.concat sep (List.map (fun (v, t) ->
      if tags then v ^ "@" ^ (string_of_int t) else v)
      vars)

and string_of_bindings ?tags:(tags=false) bindings =
  String.concat " "
    (List.map (fun ((var, t), value) ->
         "(" ^ var ^
         (if tags then "@" ^ (string_of_int t) else "") ^
         " " ^ (string_of_node ~tags value) ^ ")") bindings)

(* Extract all the tags that are contained in a node *)
let rec extract_tags = function
  | (Identifier _, t)
  | (String _, t)
  | (Integer _, t)
  | (Boolean _, t)
  | (Nil, t)
  | (Locked, t)
  | (Unlocked, t) ->
    [t]
  | (Funcall (f, args), t) ->
    t :: ((extract_tags f) @ (Util.flatmap extract_tags args))
  | (Lambda (vars, body), t) ->
    t :: ((List.map (fun (_, t) -> t) vars) @
          (Util.flatmap extract_tags body))
  | (Begin body, t) ->
    t :: (Util.flatmap extract_tags body)
  | (LetRec (bindings, body), t) ->
    t :: (Util.flatmap (fun ((_, t), n) -> t :: extract_tags n) bindings) @
    (Util.flatmap extract_tags body)
  | (If (cond, cons, alt), t) ->
    t :: (Util.flatmap extract_tags [cond; cons; alt])
  | (Set ((_, t'), exp), t) ->
    t :: t' :: (extract_tags exp)
  | (Callcc exp, t)
  | (Spawn exp, t)
  | (Join exp, t) ->
    t :: (extract_tags exp)
  | (Cas ((_, t'), eold, enew), t) ->
    t :: t' :: ((extract_tags eold) @ (extract_tags enew))
  | (Acquire (_, t'), t)
  | (Release (_, t'), t) ->
    [t; t']


(* Try to find a node corresponding to a tag in a given node *)
let rec find_node tag node =
  (* mplus operation on the option monad. Unfortunately, it forces the
   * evaluation of both arguments (it shouldn't be a problem, but if it is,
   * adding some laziness (lazy and Lazy.force) will probably do the trick) *)
  let (++) x y = match x with
    | None -> y
    | Some _ -> x in
  (* Find a node among a list of nodes *)
  let find_node' nodes =
    List.fold_left (++) None (List.map (find_node tag) nodes) in
  match node with
  | (_, tag') when tag' = tag -> Some node
  | (Identifier _, _)
  | (String _, _)
  | (Integer _, _)
  | (Boolean _, _)
  | (Nil, _)
  | (Locked, _)
  | (Unlocked, _)
  | (Acquire _, _)
  | (Release _, _) ->
    None
  | (Funcall (f, args), _) ->
    find_node tag f ++ find_node' args
  | (Lambda (_, body), _)
  | (Begin body, _) ->
    find_node' body
  | (LetRec (bindings, body), _) ->
    List.fold_left (++) None
      (List.map (fun (_, n) -> find_node tag n) bindings) ++
    find_node' body
  | (If (cond, cons, alt), _) ->
    find_node tag cond ++ find_node tag cons ++ find_node tag alt
  | (Set (_, e), _)
  | (Callcc e, _)
  | (Spawn e, _)
  | (Join e, _) ->
    find_node tag e
  | (Cas (_, e1, e2), _) ->
    find_node tag e1 ++ find_node tag e2

(* Find the tag with the highest number in a node *)
let rec highest_tag node =
  let highest_var_tag vars =
    List.fold_left max 0 (List.map snd vars)
  and highest_binding_tag bindings =
    List.fold_left max 0 (List.map (fun ((_, t), n) -> max t (highest_tag n))
                            bindings)
  and highest_tag' nodes =
    List.fold_left max 0 (List.map highest_tag nodes) in
  match node with
  | (Identifier _, tag)
  | (String _, tag)
  | (Integer _, tag)
  | (Boolean _, tag)
  | (Nil, tag)
  | (Locked, tag)
  | (Unlocked, tag) ->
    tag
  | (Acquire (_, tag'), tag)
  | (Release (_, tag'), tag) ->
    max tag tag'
  | (Funcall (f, args), tag) ->
    max tag (max (highest_tag f) (highest_tag' args))
  | (Lambda (args, body), tag) ->
    max tag (max (highest_var_tag args) (highest_tag' body))
  | (Begin body, tag) ->
    max tag (highest_tag' body)
  | (LetRec (bindings, body), tag) ->
    max tag (max (highest_binding_tag bindings) (highest_tag' body))
  | (If (cond, cons, alt), tag) ->
    max tag (max (max (highest_tag cond) (highest_tag cons))
               (highest_tag alt))
  | (Set ((_, tag'), value), tag) ->
    max tag (max tag' (highest_tag value))
  | (Callcc n, tag)
  | (Spawn n, tag)
  | (Join n, tag) ->
    max tag (highest_tag n)
  | (Cas ((_, tag'), n1, n2), tag) ->
    max tag (max tag' (max (highest_tag n1) (highest_tag n2)))
