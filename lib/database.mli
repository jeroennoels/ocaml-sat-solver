open! Base

type t

val create : Cnf.t -> t
val get_clause : t -> Clause_id.t -> Literal.t array

(** A clause is relevant for a literal when it contains that literal's negation. *)
val relevant_clauses : t -> Literal.t -> Clause_id.t array
