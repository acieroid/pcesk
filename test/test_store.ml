open OUnit2
open Types
open Cesk_types
open Cesk_base

let test_latval name value expected =
  assert_equal ~msg:name ~printer:Lattice.string_of_lattice_value
    expected value

let test_alloc ctx =
  let addr = (TagAddr (1, IntTime 0))
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
  let addr = (TagAddr (1, IntTime 0))
  and store = Store.empty
  and value = Lattice.abst1 (AbsUnique (Integer 1)) in
  let store_allocated = Store.alloc store addr value in
  let value_looked_up = Store.lookup store_allocated addr in
  test_latval "alloc 1" value_looked_up value;

  let value' = Lattice.abst1 (AbsUnique (Integer 2)) in
  let store_allocated' = Store.update store_allocated addr value' in
  let value_looked_up = Store.lookup store_allocated' addr in
  test_latval "alloc 1, update 2" value_looked_up value'

let suite =
  "Store tests" >:::
    ["alloc" >:: test_alloc;
     "update" >:: test_update;
    ]
