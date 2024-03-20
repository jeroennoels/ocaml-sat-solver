open! Base

type t

val of_int_check : nbvar:int -> int -> t
val of_int_unchecked : int -> t
val to_int : t -> int
val equal : t -> t -> bool
val of_literal : Literal.t -> t
val to_literal : t -> bool -> Literal.t
val show : t -> string
