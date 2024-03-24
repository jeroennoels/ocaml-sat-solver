open! Base

type t

val analyze_conflict : Database.t -> Trail.t -> Conflict.t -> t
val get_conflict_clause : t -> Literal.t array

(* exposed for testing and debugging *)
val get_conflict_variable : t -> Variable.t
val get_num_steps : t -> int
val partition : t -> Variable.t -> Literal.t array -> int list
val index : t -> Variable.t -> int option
val print : t -> unit
