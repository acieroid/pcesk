open OUnit
open Types
open Lattice1

let suite =
  "Lattice tests" >:::
    ["lattice1" >:: Lattice1.test
    ]
