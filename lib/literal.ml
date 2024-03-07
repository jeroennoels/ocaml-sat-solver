open! Base

type t = int

let of_int_check bound x =
  assert (Int.abs x <= bound && not (x = 0));
  x
;;

let to_int x = x [@@inline]
let negate x = -x [@@inline]
let show x = Int.to_string x [@@inline]
