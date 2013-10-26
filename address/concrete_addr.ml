open Address

module Concrete_addr : ADDRESS =
  struct

    type t = int

    let first = 0

    let next addr = addr + 1

    let compare = Pervasives.compare

    let string_of_address = string_of_int

  end
