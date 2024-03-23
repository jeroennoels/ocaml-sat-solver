open! Base

type t

val analyze_conflict : Database.t -> Trail.t -> Conflict.t -> t

(* exposed for testing and debugging *)
val get_conflict_variable : t -> Variable.t
val partition : t -> Variable.t -> Literal.t array -> int list
val index : t -> Variable.t -> int option
val print : t -> unit
