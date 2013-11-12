open Params

let suites = [
  Test_ast.suite;
  Test_lattice.suite;
  Test_simple.suite;
  Test_advanced.suite;
]

let () =
  Arg.parse speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
  print_string ("Running tests with: \n" ^ (string_of_configuration ()) ^ "\n");
  let _ = List.map OUnit2.run_test_tt_main suites in
  ()
