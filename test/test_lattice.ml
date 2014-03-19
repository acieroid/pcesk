open OUnit2
open Types
open Set_lattice

module Set_lattice_5 = Set_lattice(struct let size = 5 end)

let suite =
  "Lattice tests" >:::
  ["set lattice" >:: Set_lattice_5.test;
  ]
