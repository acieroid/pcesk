open OUnit
open Types

let (=>) string expected =
  let node = Scheme_parser.parse (Scheme_lexer.lex_string string) in
  let res, _ = Cesk.eval node in
  assert_equal (List.length res) 1;
  let r, _, _ = List.hd res in
  assert_equal ~msg:string ~printer:string_of_value expected r


let test_multiple_calls () =
  "(begin (define inc (lambda (x) (+ x 1))) (inc (inc 2)))" => AbsInteger

let test_recursive_calls () =
  "(begin
       (define count (lambda (n) (if (= n 0) \"done\" (count (- n 1)))))
       (count 200))" => AbsUnique (String "done")

let suite =
  "Advanced tests" >:::
    ["multiple calls" >:: test_multiple_calls;
     "recursive calls" >:: test_recursive_calls;
    ]


