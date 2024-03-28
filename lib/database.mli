open! Base

type t

val create : Cnf.t -> t
val num_variables : t -> int
val get_literals : t -> Clause_id.t -> Literal.t array

val fold_over_clauses_containing
  :  t
  -> Literal.t
  -> f:('a list -> Literal.t array -> Clause_id.t -> 'a list)
  -> 'a list

val add_learned_clause : t -> Literal.t array -> Clause_id.t

(* for testing only *)
val get_clause_ids : t -> Literal.t -> Clause_id.t array
