open! Base

type t

val create : Cnf.t -> t
val relevant_clauses : t -> Literal.t -> Clause_id.t array
