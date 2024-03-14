open! Base

type antecedent = Literal.t * Clause_id.t
type t

val empty : unit -> t
val dequeue : t -> antecedent option
val enqueue_all : t -> antecedent list -> antecedent option
