(* k of the k-CFA *)
let k = ref 0

(* GC turned on or not *)
let gc = ref false

(* Verbosity level *)
let verbose = ref 0

(* Verbosity level at which debug messages will be printed *)
let debug_level = 2

(* Input file *)
let input = ref stdin

(* Output for the dot file *)
let graph_file = ref None

(* Print only useful information *)
let quiet = ref false

let usage = "usage: " ^ (Sys.argv.(0)) ^
              " [-v level] [-i input] [-g graph_output] [-k polyvariance] [-gc]"

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
   ": turn on abstract garbage collection (disabled by default)");
  ("-quiet", Arg.Set quiet,
   ": don't print the results nor the parameters used, only the time and graph size");
]


let string_of_configuration () =
  "\tk = " ^ (string_of_int !k) ^ "\n" ^
    "\tgc = " ^ (if !gc then "on" else "off")
