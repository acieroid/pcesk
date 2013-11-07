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

let test_match string expected cmp =
  let node = Scheme_parser.parse (Scheme_lexer.lex_string string) in
  let res, _ = Cesk.eval node in
  let results = List.map (fun (r, _, _) -> r) res in
  let merged = Lattice.abst results in
  assert_equal
    ~cmp
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
  "(begin
     (define g (lambda ()
       1))
     (define f (lambda (n)
       (if (= n 0)
         0
         (+ (f (- n 1)) (g)))))
     (f 10))" => AbsInteger

let test_church_numerals () =
  let church x =
    "(begin
  (define zero
    (lambda (f x)
      x))
    (define inc
      (lambda (n)
        (lambda (f x)
          (f (n f x)))))
    (define plus
      (lambda (m n)
        (lambda (f x)
          (m f (n f x)))))" ^ x ^ ")" in
  let test_clo s =
    test_match (church s)
      (AbsUnique (Closure (([], []), Cesk_base.empty_env)))
      (fun (x : Lattice.t) (y : Lattice.t) ->
         List.exists (function
             | AbsUnique (Closure _) -> true
             | _  -> false)
           (Lattice.conc y)); in
  test_clo "zero";
  test_clo "(inc zero)";
  test_clo "(plus (inc (inc (inc zero))) (plus (inc (inc zero)) (inc zero)))"

let test_infinite () =
  let test_no_result string =
    let node = Scheme_parser.parse (Scheme_lexer.lex_string string) in
    let res, _ = Cesk.eval node in
    let results = List.map (fun (r, _, _) -> r) res in
    assert_equal
      ~msg:string ~printer:(fun l ->
          "[" ^ (String.concat ", "
                   (List.map string_of_value l)) ^ "]")
      [] results in
  test_no_result "(begin (define f (lambda () (f))) (f))";
  test_no_result "(begin (define t (lambda (x) (t (+ x 1)))) (t 0))"

let suite =
  "Advanced tests" >:::
    ["multiple calls" >:: test_multiple_calls;
     "recursive calls" >:: test_recursive_calls;
     "fibonacci" >:: test_fibo;
     "widen" >:: test_widen;
     "church numerals" >:: test_church_numerals;
     "infinitely recursive functions" >:: test_infinite;
    ]
