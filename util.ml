module StringSet = Set.Make(struct
    type t = string
    let compare = Pervasives.compare
  end)

let string_set_of_list l =
  List.fold_left (fun set x -> StringSet.add x set) StringSet.empty l

let string_set_of_vars l =
  List.fold_left (fun set (x, _) -> StringSet.add x set) StringSet.empty l

let string_of_string_set s =
  "{" ^ (String.concat ", " (StringSet.elements s)) ^ "}"

let debug s =
  if !Params.verbose >= Params.debug_level then
    begin print_string (String.concat "" s); print_newline (); end