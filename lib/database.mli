open! Base

type t

val create : Cnf.t -> t
val get_clause : t -> Clause_id.t -> Literal.t array
val relevant_clauses : t -> Literal.t -> Clause_id.t array
