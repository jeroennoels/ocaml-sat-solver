open! Base

type t = int

let of_int_check bound x =
  assert (x > 0 && x <= bound);
  x
;;

let to_int x = x [@@inline]
let of_literal x = Int.abs (Literal.to_int x)
let show x = Int.to_string x [@@inline]
