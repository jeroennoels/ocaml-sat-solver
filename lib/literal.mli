open! Base

type t

val of_int_check : nbvar:int -> int -> t
val to_int : t -> int
val negate : t -> t
val is_positive : t -> bool
val equal : t -> t -> bool
val same_variable : t -> t -> bool
val of_int_unchecked : int -> t
val show : t -> string
val array_of_int_set : (int, Int.comparator_witness) Set.t -> t array
