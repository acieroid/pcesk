open OUnit
open Types

let (=>) string expected =
  let node = Scheme_parser.parse (Scheme_lexer.lex_string string) in
  let res, _ = Cesk.eval node in
  assert_equal (List.length res) 1;
  let r, _, _ = List.hd res in
  assert_equal ~msg:string ~printer:string_of_value (AbsUnique expected) r

let test_atoms () =
  "1" => Integer 1;
  "\"foo\"" => String "foo";
  "#t" => Boolean true;
  "#f" => Boolean false

let test_lambda1 () =
  "((lambda (x) x) 42)" => Integer 42

let test_lambda2 () =
  "((lambda (x y) x) 1 2)" => Integer 1;
  "((lambda (x y) y) 1 2)" => Integer 2

let test_begin () =
  "(begin 1 2 3)" => Integer 3;
  "(begin (+ 1 2) (+ 2 3) (+ 3 (begin 4 5)))" => Integer 8

let test_define_simple () =
  "(begin (define x 1) x)" => Integer 1;
  "(begin (define x (+ 1 2)) (define y (+ x x)) y)" => Integer 6;
  "(begin (define x 1))" => Unspecified

let test_if () =
  "(if #t 1 2)" => Integer 1;
  "(if #f 1 2)" => Integer 2

let test_set () =
  "(begin (define x 1) (set! x 2) x)" => Integer 2;
  "(begin (define x 1) (set! x (+ x x)) x)" => Integer 2

let test_primitives () =
  "(+ 1)" => Integer 1;
  "(+ 1 2)" => Integer 3;
  "(+ 1 2 3)" => Integer 6;
  "(* 1 2)" => Integer 2;
  "(* 2 3 7)" => Integer 42;
  "(- 1)" => Integer (-1);
  "(- 1 2)" => Integer (-1);
  "(- 1 2 3)" => Integer (-4);
  "(= 5 (+ 2 3))" => Boolean true;
  "(= 2 3)" => Boolean false;
  "(> 1 2)" => Boolean false;
  "(> 2 1)" => Boolean true;
  "(> 1 1)" => Boolean false;
  "(>= 1 1)" => Boolean true

let suite =
  "Simple tests" >:::
    ["atom" >:: test_atoms;
     "lambda with one argument" >:: test_lambda1;
     "lambda with two arguments" >:: test_lambda2;
     "begin" >:: test_begin;
     "simple define" >:: test_define_simple;
     "if" >:: test_if;
     "set" >:: test_set;
     "primitives" >:: test_primitives;
    ]
