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

let test_tags ctx =
  let all_uniques l = match List.sort Pervasives.compare l with
    | [] -> true
    | hd::tl ->
      fst (List.fold_left (fun (acc, last) el -> (acc && (el <> last), el))
             (true, hd) tl) in
  let unique_tags string =
    let node = Parser.parse (Lexer.lex_string string) in
    assert_bool string (all_uniques (Ast.extract_tags node)) in
  unique_tags "(lambda (x) x)";
  unique_tags "(begin \"foo\" \"bar\" 1 #t #f 0)";
  unique_tags ("(letrec ((f (lambda (x) (+ x 1))) (g (lambda (x) (- x 1)))) " ^
               "(+ (f 1) (g 1)))");
  unique_tags "(if (> (* 3 2) 5) (+ (* 3 8) 4) (begin (+ 1 2) (* 2 4)))";
  unique_tags "(letrec ((x 1) (y 2) (z 3)) (set! x (+ x y (- z y))))";
  unique_tags "(letrec ((t (spawn (letrec ((x 1)) (set! x 2) x)))) (join t))";
  unique_tags "(letrec ((x 1)) (cas x (- 3 2) (* 2 8)))"

let suite =
  "AST tests" >:::
  ["free variables" >:: test_free_variables;
   "tags" >:: test_tags;
  ]
