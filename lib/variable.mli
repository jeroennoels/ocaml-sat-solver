open! Base

type t

val of_int_check : int -> int -> t
val to_int : t -> int
val of_literal : Literal.t -> t
val to_literal : t -> bool -> Literal.t
val show : t -> string
