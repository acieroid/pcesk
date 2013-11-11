(* k of the k-CFA *)
let k = ref 0

(* GC turned on or not *)
let gc = ref false

(* Verbosity level *)
let verbose = ref 0

(* Input file *)
let input = ref stdin

(* Output for the dot file *)
let graph_file = ref None

let usage = "usage: " ^ (Sys.argv.(0)) ^ " [-v] [-i input] [-g graph_output]"

let speclist = [
  ("-v", Arg.Set_int verbose,
   ": verbose level (0 by default)");
  ("-i", Arg.String (fun s -> input := open_in s),
   ": input file (stdin by default)");
  ("-g", Arg.String (fun s -> graph_file := Some s),
   ": output file for the generated graph (nothing by default)");
  ("-k", Arg.Set_int k,
   ": polyvariance (k-CFA) (k=0 by default)");
  ("-gc", Arg.Set gc,
   ": turn on abstract garbage collection (disabled by default)")
]


let string_of_configuration () =
  "\tk = " ^ (string_of_int !k) ^ "\n" ^
    "\tgc = " ^ (if !gc then "on" else "off")
