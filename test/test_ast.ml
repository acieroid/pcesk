open OUnit2
open Types
open Util
open Free_variables

let (=>) string expected =
  let node = Parser.parse (Lexer.lex_string string) in
  assert_equal ~msg:string ~printer:string_of_string_set
    expected (free_variables node)

let test_free_variables ctx =
  "(lambda (x) x)" => StringSet.empty;
  "42" => StringSet.empty;
  "#t" => StringSet.empty;
  "(letrec ((f (lambda (x) (if (= x 0) 0 (f (- x 1))))))
     (+ (f 10) 5))" => StringSet.empty;
  "(lambda (x y) (+ x y z))" => (StringSet.singleton "z");
  "(letrec ((x 1)) (set! y (+ x 1)) (if (> y z) y (+ x z)))" =>
    (string_set_of_list ["z"; "y"])

let suite =
  "AST tests" >:::
    ["free variables" >:: test_free_variables;
    ]