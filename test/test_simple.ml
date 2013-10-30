open OUnit
open Types

let run_concrete string =
  let node = Scheme_parser.parse (Scheme_lexer.lex_string string) in
  let res, _ = Cesk.eval node in
  assert_equal (List.length res) 1;
  List.hd res

let test_int _ =
  let (value, _, _) = run_concrete "1" in
  assert_equal value (Integer 1)

let test_string _ =
  let (value, _, _) = run_concrete "\"foo\"" in
  assert_equal value (String "foo")

let test_boolean _ =
  let (t, _, _) = run_concrete "#t"
  and (f, _, _) = run_concrete "#f" in
  assert_equal t (Boolean true);
  assert_equal f (Boolean false)

let test_lambda1 _ =
  let (value, _, _) = run_concrete "((lambda (x) x) 42)" in
  assert_equal value (Integer 42)

let test_lambda2 _ =
  let (v1, _, _) = run_concrete "((lambda (x y) x) 1 2)"
  and (v2, _, _) = run_concrete "((lambda (x y) y) 1 2)" in
  assert_equal v1 (Integer 1);
  assert_equal v2 (Integer 2)

let test_begin _ =
  let (v1, _, _) = run_concrete "(begin 1 2 3)"
  and (v2, _, _) = run_concrete "(begin (+ 1 2) (+ 2 3) (+ 3 (begin 4 5)))" in
  assert_equal v1 (Integer 3);
  assert_equal v2 (Integer 8)

let test_define_simple _ =
  let (v1, _, _) = run_concrete "(begin (define x 1) x)"
  and (v2, _, _) = run_concrete "(begin (define x (+ 1 2)) (define y (+ x x)) y)"
  and (v3, _, _) = run_concrete "(begin (define x 1))" in
  assert_equal v1 (Integer 1);
  assert_equal v2 (Integer 6);
  assert_equal v3 Unspecified

let test_define_fun _ =
  todo "syntactic sugar for define not implemented";
  let (v1, _, _) = run_concrete "(begin (define (f x) (+ x 1)) (f 41))" in
  assert_equal v1 (Integer 42)

let test_if _ =
  let (v1, _, _) = run_concrete "(if #t 1 2)"
  and (v2, _, _) = run_concrete "(if #f 1 2)" in
  assert_equal v1 (Integer 1);
  assert_equal v2 (Integer 2)

let test_set _ =
  let (v1, _, _) = run_concrete "(begin (define x 1) (set! x 2) x)"
  and (v2, _, _) = run_concrete "(begin (define x 1) (set! x (+ x x)) x)" in
  assert_equal v1 (Integer 2);
  assert_equal v2 (Integer 2)

let suite =
  "Simple tests" >:::
    ["integer" >:: test_int;
     "string" >:: test_string;
     "boolean" >:: test_boolean;
     "lambda with one argument" >:: test_lambda1;
     "lambda with two arguments" >:: test_lambda2;
     "begin" >:: test_begin;
     "simple define" >:: test_define_simple;
     "syntactic sugar define" >:: test_define_fun;
     "if" >:: test_if;
     "set" >:: test_set;
    ]
