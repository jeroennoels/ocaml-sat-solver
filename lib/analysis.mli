open! Base

type t

val analyze_conflict : Database.t -> Trail.t -> Conflict.t -> t
val get_learned_clause_exn : t -> Literal.t array
val get_uip_literal : t -> Literal.t option
val calculate_backjump_step : t -> int

(* exposed for testing and debugging *)
val get_conflict_variable : t -> Variable.t
val get_num_steps : t -> int
val partition : t -> Variable.t -> Literal.t array -> int list
val index : t -> Variable.t -> int option
val print : t -> unit
