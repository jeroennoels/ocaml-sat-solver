open! Base

type t

val empty : nbvar:int -> t
val assignment : t -> Assignment.t
val decide : t -> Literal.t option
val step : t -> Literal.t * Clause_id.t -> unit
val decision_level_exn : t -> Variable.t -> int
