open! Base

type t = int

let of_int_check x = if x >= 0 then x else invalid_arg "clause id"
let to_int x = x [@@inline]
let show x = Int.to_string x [@@inline]
