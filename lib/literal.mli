open! Base

type t

val of_int_check : int -> int -> t
val to_int : t -> int
val negate : t -> t
val show : t -> string
