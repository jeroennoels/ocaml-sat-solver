open! Base

type t

val num_variables : t -> int
val num_clauses : t -> int
val clauses : t -> int array array
val read_lines : string list -> (t, string) Result.t
val read_file : string -> (t, string) Result.t
