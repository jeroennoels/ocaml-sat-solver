open! Base

type t

val create : Literal.t * Clause_id.t -> Literal.t * Clause_id.t -> t
val variable : t -> Variable.t
val antecedent1 : t -> Clause_id.t
val antecedent2 : t -> Clause_id.t
val show : t -> string
