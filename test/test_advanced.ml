open OUnit
open Types
open Set_lattice

module Lattice = Set_lattice(struct let size = 5 end)

let (=>) string expected =
  let node = Scheme_parser.parse (Scheme_lexer.lex_string string) in
  let res, _ = Cesk.eval node in
  let results = List.map (fun (r, _, _) -> r) res in
  let merged = Lattice.abst results in
  assert_equal
    ~msg:string ~printer:Lattice.string_of_lattice_value
    (Lattice.abst1 expected) merged


let test_multiple_calls () =
  "(begin
     (define inc (lambda (x)
       (+ x 1)))
      (inc (inc 2)))" => AbsInteger

let test_recursive_calls () =
  "(begin
     (define count (lambda (n)
       (if (= n 0)
         \"done\"
         (count (- n 1)))))
     (count 200))" => AbsUnique (String "done");
  "(begin
     (define fact (lambda (n)
       (if (= n 0)
         1
         (* n (fact (- n 1))))))
     (fact 5))" => AbsInteger

let suite =
  "Advanced tests" >:::
    ["multiple calls" >:: test_multiple_calls;
     "recursive calls" >:: test_recursive_calls;
    ]


