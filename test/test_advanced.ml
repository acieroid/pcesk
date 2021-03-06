open OUnit2
open Types
open Set_lattice

module Lattice = Set_lattice(struct let size = 5 end)

let (=>) string expected =
  let node = Parser.parse (Lexer.lex_string string) in
  let res, _ = Cesk.eval node in
  let results = List.map (fun (r, _, _) -> r) res in
  let merged = Lattice.abst results in
  assert_equal
    ~msg:string ~printer:Lattice.string_of_lattice_value
    (Lattice.abst1 expected) merged

let ($=>) f expected =
  let node = Parser.parse (Lexer.lex_input_file f) in
  let res, _ = Cesk.eval node in
  let results = List.map (fun (r, _, _) -> r) res in
  let merged = Lattice.abst results in
  let cmp x y = not (Lattice.is_bottom (Lattice.meet x y)) in
  assert_equal
    ~msg:f ~cmp ~printer:Lattice.string_of_lattice_value
    (Lattice.abst1 expected) merged

let test_match f expected cmp =
  let node = Parser.parse (Lexer.lex_input_file f) in
  let res, _ = Cesk.eval node in
  let results = List.map (fun (r, _, _) -> r) res in
  let merged = Lattice.abst results in
  assert_equal
    ~cmp
    ~msg:f ~printer:Lattice.string_of_lattice_value
    (Lattice.abst1 expected) merged

let test_multiple_calls ctx =
  "seq/sq.scm" $=> AbsInteger;
  "seq/inc.scm" $=> AbsInteger

let test_recursive_calls ctx =
  (* TODO: I don't know why, but there is a "bad file descriptor" exception
   * before lexing the string "done" when reading from count.scm, and only when
   * it is made from the test suite (running with -i input/count.scm works
   * perfectly). *)
  "(letrec ((count (lambda (n)
                  (if (= n 0)
                    \"done\"
                    (count (- n 1))))))
  (count 200))" => AbsString;
  "seq/fact.scm" $=> AbsInteger

let test_fibo ctx =
  "seq/fib.scm" $=> AbsInteger

let test_widen ctx =
  (* If the CESK machine does not widen the values at a certain points, this
   * example will keep running, with values staying at the same "level" of the
   * lattice, but with different values (in case thelattice as an infinite
   * width) *)
  "seq/widen.scm" $=> AbsInteger

let test_church_numerals ctx =
  let test_clo f =
    test_match f
      (AbsUnique (Closure (([], []), Cesk_base.empty_env)))
      (fun x y ->
         List.exists (function
             | AbsUnique (Closure _) -> true
             | _  -> false)
           (Lattice.conc y)); in
  test_clo "seq/church-0.scm";
  test_clo "seq/church-1.scm";
  test_clo "seq/church-2.scm";
  test_clo "seq/church-6.scm";
  "seq/church-2-num.scm" $=> AbsInteger

let test_infinite ctx =
  let test_no_result f =
    let node = Parser.parse (Lexer.lex_input_file f) in
    let res, _ = Cesk.eval node in
    let results = List.map (fun (r, _, _) -> r) res in
    assert_equal
      ~msg:f ~printer:(fun l ->
          "[" ^ (String.concat ", "
                   (List.map string_of_value l)) ^ "]")
      [] results in
  test_no_result "seq/infinite-1.scm";
  test_no_result "seq/infinite-2.scm"

let test_callcc ctx =
  "seq/callcc-0.scm" $=> AbsInteger

let test_blur ctx =
  "seq/blur.scm" $=> AbsBoolean

let test_cpstak ctx =
  "seq/cpstak.scm" $=> AbsInteger

let test_eta ctx =
  "seq/eta.scm" $=> AbsBoolean

let test_gcipd ctx =
  "seq/gcipd.scm" $=> AbsInteger

let test_kcfa2 ctx =
  "seq/kcfa2.scm" $=> AbsBoolean

let test_kcfa3 ctx =
  "seq/kcfa3.scm" $=> AbsBoolean

let test_mj09 ctx =
  "seq/mj09.scm" $=> AbsInteger

let test_rotate ctx =
  "seq/rotate.scm" $=> AbsString

let suite =
  "Advanced tests" >:::
  ["multiple calls" >:: test_multiple_calls;
   "recursive calls" >:: test_recursive_calls;
   "fibonacci" >:: test_fibo;
   "widen" >:: test_widen;
   "church numerals" >:: test_church_numerals;
   "infinitely recursive functions" >:: test_infinite;
   "call/cc" >:: test_callcc;
   "blur" >:: test_blur;
   (* Parser problem (only when ran from OUnit) *)
   (* "CPS tak" >:: test_cpstak; *)
   "eta" >:: test_eta;
   "gcipd" >:: test_gcipd;
   "kcfa2" >:: test_kcfa2;
   "kcfa3" >:: test_kcfa3;
   "mj09" >:: test_mj09;
   (* Parser problem *)
   (* "rotate" >:: test_rotate; *)
  ]
