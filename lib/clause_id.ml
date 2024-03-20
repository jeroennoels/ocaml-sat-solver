open! Base

type t = int

let of_int x =
  assert (x >= 0);
  x
[@@inline]
;;

let to_int x = x [@@inline]
let invalid_as_int = -1
let show x = Int.to_string x [@@inline]
