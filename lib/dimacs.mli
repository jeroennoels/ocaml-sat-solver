open! Base

type t

val read_lines : string list -> (Cnf.t, string) Result.t
val read_file : string -> (Cnf.t, string) Result.t
