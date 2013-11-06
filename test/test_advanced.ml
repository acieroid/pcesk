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
     (define sq (lambda (x)
       (* x x)))
     (sq 2)
     (sq 3))" => AbsInteger;
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
     (count 200))" => AbsString;
  "(begin
     (define fact (lambda (n)
       (if (= n 0)
         1
         (* n (fact (- n 1))))))
     (fact 5))" => AbsInteger

let test_fibo () =
  "(begin
     (define fib (lambda (n)
       (if (< n 2)
         n
         (+ (fib (- n 1)) (fib (- n 2))))))
     (fib 4))" => AbsInteger

let test_widen () =
  (* If the CESK machine does not widen the values at a certain points,
     this example will keep running, with values staying at the same
     "level" of the lattice, but with different values (in case the
     lattice as an infinite width) *)
  todo "1+2 -> Int";
  "(begin
     (define g (lambda ()
       1))
     (define f (lambda (n)
       (if (= n 0)
         0
         (+ (f (- n 1)) (g)))))
     (f 10))" => AbsInteger

let suite =
  "Advanced tests" >:::
    ["multiple calls" >:: test_multiple_calls;
     "recursive calls" >:: test_recursive_calls;
     "fibonacci" >:: test_fibo;
     "widen" >:: test_widen;
    ]


