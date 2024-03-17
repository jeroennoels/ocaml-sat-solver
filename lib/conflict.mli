open! Base

type t

val create : Literal.t * Clause_id.t -> Clause_id.t -> t
val get_step : t -> Literal.t * Clause_id.t
val get_kappa : t -> Clause_id.t
val show : t -> string
