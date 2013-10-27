open OUnit
open Types

let run string =
  let node = Scheme_parser.parse (Scheme_lexer.lex_string string) in
  Cesk.eval node

let test_int _ =
  let (value, _, _) = run "1" in
  assert_equal value (Integer 1)

let test_string _ =
  let (value, _, _) = run "\"foo\"" in
  assert_equal value (String "foo")

let test_boolean _ =
  let (t, _, _) = run "#t"
  and (f, _, _) = run "#f" in
  assert_equal t (Boolean true);
  assert_equal f (Boolean false)

let test_lambda1 _ =
  let (value, _, _) = run "((lambda (x) x) 42)" in
  assert_equal value (Integer 42)

let test_lambda2 _ =
  let (v1, _, _) = run "((lambda (x y) x) 1 2)"
  and (v2, _, _) = run "((lambda (x y) y) 1 2)" in
  assert_equal v1 (Integer 1);
  assert_equal v2 (Integer 2)

let suite =
  "Simple tests" >:::
    ["integer" >:: test_int;
     "string" >:: test_string;
     "boolean" >:: test_boolean;
     "lambda with one argument" >:: test_lambda1;
     "lambda with two arguments" >:: test_lambda2;
    ]
