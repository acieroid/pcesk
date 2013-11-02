open OUnit
open Types
(* open Lattice1 *)
(* open Cp_lattice *)
open Set_lattice

module Set_lattice_5 = Set_lattice(struct let size = 5 end)

let suite =
  "Lattice tests" >:::
    [(* "lattice1" >:: Lattice1.test;
     "cp lattice" >:: Cp_lattice.test; *)
     "set lattice" >:: Set_lattice_5.test;
    ]
