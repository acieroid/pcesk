open OUnit
open Types
open Lattice1
open Cp_lattice

let suite =
  "Lattice tests" >:::
    ["lattice1" >:: Lattice1.test;
     "cp lattice" >:: Cp_lattice.test;
    ]
