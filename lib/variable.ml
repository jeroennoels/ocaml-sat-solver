open! Base

type t = int

let of_int_check ~nbvar x =
  assert (x > 0 && x <= nbvar);
  x
;;

let of_int_unchecked x = x [@@inline]
let to_int x = x [@@inline]
let of_literal x = Int.abs (Literal.to_int x)

let to_literal x = function
  | false -> Literal.of_int_unchecked (-x)
  | true -> Literal.of_int_unchecked x
;;

let show x = Int.to_string x [@@inline]
