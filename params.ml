(* k of the k-CFA *)
let k = ref 0

(* GC turned on or not *)
let gc = ref false

(* Parallelism turned on or not *)
let parallel = ref false

(* Remove threads of the state when they halt *)
let remove_threads = ref false

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

(* Possible targets *)
type target = PrintAST | Run

let target = ref Run

let usage = "usage: " ^ (Sys.argv.(0)) ^
              " [-v level] [-i input] [-g graph_output] [-k polyvariance]" ^
              " [-gc] [-p] [-quiet] [-target target]"

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
  ("-p", Arg.Set parallel,
   ": turn on parallelism with spawn and join (disabled by default)");
  (* See A Family of Abstract Interpretation for Static Analysis of Concurrent
     Higher Order Programs, p. 11: "It is worth asking whether it is sound in
     just this case to remove the context from the threads. [...]". This
     parameter allows to tweak this. *)
  ("-r", Arg.Set remove_threads,
   ": remove threads when they halt (disabled by default)");
  ("-quiet", Arg.Set quiet,
   ": don't print the results nor the parameters used, only the time and graph size");
  ("-target", Arg.Symbol (["run"; "ast"],
                          (function
                            | "run" -> target := Run
                            | "ast" -> target := PrintAST
                            | t -> failwith ("Invalid target: " ^ t))),
   ": action to do with the input ('run' or 'ast', 'run' by default)");
]

let string_of_param name value =
  name ^ " = " ^ value

let string_of_bool_param name value =
  string_of_param name (if value then "on" else "off")

let string_of_configuration () =
  "\t" ^ (String.concat "\n\t"
            [string_of_param "k" (string_of_int !k);
             string_of_bool_param "gc" !gc;
             string_of_bool_param "parallelism" !parallel;
             string_of_bool_param "remove_threads" !remove_threads;
            ])
