open OUnit2
open Types
open Cesk_types
open Cesk_base

let test_latval name value expected =
  assert_equal ~msg:name ~printer:Lattice.string_of_lattice_value
    expected value

let test_alloc ctx =
  let addr = TagAddr (1, IntTime 0)
  and store = Store.empty
  and value = Lattice.abst1 (AbsUnique (Integer 1)) in
  let store_allocated = Store.alloc store addr value in
  let value_looked_up = Store.lookup store_allocated addr in
  test_latval "alloc 1" value_looked_up value;

  let value' = Lattice.abst1 (AbsUnique (Integer 2)) in
  let store_allocated' = Store.alloc store_allocated addr value' in
  let value_looked_up = Store.lookup store_allocated' addr in
  test_latval "alloc 1, alloc 2" value_looked_up (Lattice.join value value')

let test_update ctx =
  let addr = TagAddr (1, IntTime 0)
  and store = Store.empty
  and value = Lattice.abst1 (AbsUnique (Integer 1)) in
  let store_allocated = Store.alloc store addr value in
  let value_looked_up = Store.lookup store_allocated addr in
  test_latval "alloc 1" value_looked_up value;

  let value' = Lattice.abst1 (AbsUnique (Integer 2)) in
  let store_allocated' = Store.update store_allocated addr value' in
  let value_looked_up = Store.lookup store_allocated' addr in
  test_latval "alloc 1, update 2" value_looked_up value'

let test_store_comp op_name op s1 s2 expected =
  assert_equal ~msg:(Store.string_of_store s1 ^ " " ^ op_name ^ " " ^
                       Store.string_of_store s2)
    ~printer:string_of_bool
    expected (op s1 s2)

let test_subsumes ctx =
  let addr1 = TagAddr (1, IntTime 0)
  and addr2 = TagAddr (2, IntTime 1)
  and one = Lattice.abst1 (AbsUnique (Integer 1))
  and two = Lattice.abst1 (AbsUnique (Integer 2))
  and int = Lattice.abst1 AbsInteger in
  let s1 = Store.alloc Store.empty addr1 one
  and s2 = Store.alloc (Store.alloc Store.empty addr1 one) addr2 two
  and s3 = Store.alloc Store.empty addr1 int
  and s4 = Store.alloc Store.empty addr1 two
  and s5 = Store.alloc (Store.alloc Store.empty addr1 int) addr2 int in
  test_store_comp "subsumes" Store.subsumes s2 s1 true;
  test_store_comp "subsumes" Store.subsumes s1 s2 false;
  test_store_comp "subsumes" Store.subsumes s3 s1 true;
  test_store_comp "subsumes" Store.subsumes s3 s2 false;
  test_store_comp "subsumes" Store.subsumes s3 s4 true;
  test_store_comp "subsumes" Store.subsumes s5 s1 true;
  test_store_comp "subsumes" Store.subsumes s5 s2 true;
  test_store_comp "subsumes" Store.subsumes s5 s3 true;
  test_store_comp "subsumes" Store.subsumes s5 s4 true

let suite =
  "Store tests" >:::
    ["alloc" >:: test_alloc;
     "update" >:: test_update;
     "subsumes" >:: test_subsumes;
    ]
