open! Base

type t

val of_int_check : nbvar:int -> int -> t
val to_int : t -> int
val negate : t -> t
val is_positive : t -> bool
val of_int_unchecked : int -> t
val show : t -> string
