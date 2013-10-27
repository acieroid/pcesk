let suites = [
  Test_simple.suite;
]

let _ =
  let _ = List.map OUnit.run_test_tt_main suites in
  ()
