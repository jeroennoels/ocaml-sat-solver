open! Base

type t = int

let of_int_check ~nbvar x =
  assert (Int.abs x <= nbvar && not (x = 0));
  x
;;

let to_int x = x [@@inline]
let negate x = -x [@@inline]
let is_positive x = x > 0 [@@inline]
let show x = Int.to_string x [@@inline]
let of_int_unchecked x = x [@@inline]
