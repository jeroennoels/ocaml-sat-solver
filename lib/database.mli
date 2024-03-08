open! Base

type t

val create : Cnf.t -> t
val get_literals : t -> Clause_id.t -> Literal.t array
val get_clause_ids : t -> Literal.t -> Clause_id.t array
