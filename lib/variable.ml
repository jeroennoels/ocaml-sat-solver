open! Base

type t = int

let of_int_check bound x =
  assert (x > 0 && x <= bound);
  x
;;

let to_int x = x [@@inline]
let show x = Int.to_string x [@@inline]
