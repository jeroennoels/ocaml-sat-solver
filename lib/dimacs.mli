open! Base

type t

val read_lines : string list -> (Cnf_formula.t, string) Result.t
val read_file : string -> (Cnf_formula.t, string) Result.t
