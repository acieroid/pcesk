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

let flatmap f l =
  List.flatten (List.map f l)

(* composition law in the ordering monoid (whose possible values are
  "negative integer", "zero", "positive integer") *)
let order_comp x y =
  if x = 0 then y else x

let order_concat l = List.fold_left order_comp 0 l
