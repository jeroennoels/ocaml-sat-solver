open! Base

type t

val create : int -> int array array -> t
val num_variables : t -> int
val num_clauses : t -> int
val clauses : t -> int array array
val conflicting : (int -> bool option) -> int array -> bool

type counters

val num_satisfied : counters -> int
val num_conflicting : counters -> int
val num_undecided : counters -> int
val show_counters : counters -> string
val evaluate : t -> (int -> bool option) -> counters
