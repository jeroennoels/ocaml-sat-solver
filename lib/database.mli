open! Base

type t

val create : Cnf.t -> t
val num_variables : t -> int
val get_literals : t -> Clause_id.t -> Literal.t array
val get_clause_ids : t -> Literal.t -> Clause_id.t array
val add_learned_clause : t -> Literal.t array -> Clause_id.t
