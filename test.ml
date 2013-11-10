let suites = [
  Test_ast.suite;
  Test_lattice.suite;
  Test_simple.suite;
  Test_advanced.suite;
]

let _ =
  let _ = List.map OUnit.run_test_tt_main suites in
  ()
