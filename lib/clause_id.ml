open! Base

type t = int

let of_int x = x [@@inline]
let to_int x = x [@@inline]
let show x = Int.to_string x [@@inline]
