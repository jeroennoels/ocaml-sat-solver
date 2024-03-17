open! Base

type antecedent = Literal.t * Clause_id.t
type t

val create : unit -> t
val clear : t -> unit
val is_empty : t -> bool
val dequeue : t -> antecedent option
val enqueue_all : t -> antecedent list -> Conflict.t option
val length : t -> int
val set_logging : t -> bool -> unit
