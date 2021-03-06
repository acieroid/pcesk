open OUnit2
open Types

let (=>) string expected =
  let node = Parser.parse (Lexer.lex_string string) in
  let res, _ = Cesk.eval node in
  assert_equal (List.length res) 1;
  let r, _, _ = List.hd res in
  let cmp x y = match merge x y with
    | Some v -> true
    | None -> false in
  assert_equal ~cmp ~msg:string ~printer:string_of_value (AbsUnique expected) r

let test_atoms ctx =
  "1" => Integer 1;
  "\"foo\"" => String "foo";
  "#t" => Boolean true;
  "#f" => Boolean false

let test_lists ctx =
  "nil" => Nil;
  "(cons 1 2)" => Cons (AbsInteger, AbsInteger);
  "(cons 1 (cons 2 nil))" => Cons (AbsInteger,
                                   AbsUnique (Cons (AbsInteger,
                                                    AbsUnique Nil)));
  "(car (cons 1 2))" => Integer 1;
  "(cdr (cons 1 2))" => Integer 2;
  "(cdr (cons 1 nil))" => Nil;
  "(empty? nil)" => Boolean true;
  "(empty? (cons 1 2))" => Boolean false

let test_lambda1 ctx =
  "((lambda (x) x) 42)" => Integer 42

let test_lambda2 ctx =
  "((lambda (x y) x) 1 2)" => Integer 1;
  "((lambda (x y) y) 1 2)" => Integer 2

let test_begin ctx =
  "(begin 1 2 3)" => Integer 3;
  "(begin (+ 1 2) (+ 2 3) (+ 3 (begin 4 5)))" => Integer 8

let test_letrec ctx =
  "(letrec ((x 1)) x)" => Integer 1;
  "(letrec ((x 1) (y x)) y)" => Integer 1

let test_if ctx =
  "(if #t 1 2)" => Integer 1;
  "(if #f 1 2)" => Integer 2

let test_set ctx =
  "(letrec ((x 1)) (set! x #t))" => Unspecified;
  "(letrec ((x 1)) (set! x 2) x)" => Integer 2;
  "(letrec ((x 1)) (set! x (+ x x)) x)" => Integer 2

let test_primitives ctx =
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

let test_cas ctx =
  "(letrec ((x #t))
  (cas x #t 1))" => Boolean true;
  "(letrec ((x #t))
  (cas x #t 1)
  x)" => Integer 1;
  "(letrec ((x #f))
  (cas x #t 1)
  x)" => Boolean false

let suite =
  "Simple tests" >:::
  ["atom" >:: test_atoms;
   "lists" >:: test_lists;
   "lambda with one argument" >:: test_lambda1;
   "lambda with two arguments" >:: test_lambda2;
   "begin" >:: test_begin;
   "letrec" >:: test_letrec;
   "if" >:: test_if;
   "set" >:: test_set;
   "primitives" >:: test_primitives;
  ]
