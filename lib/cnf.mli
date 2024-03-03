open! Base

type t

val create : int -> int array array -> t
val num_variables : t -> int
val num_clauses : t -> int
val clauses : t -> int array array
