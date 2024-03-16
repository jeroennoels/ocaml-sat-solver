open! Base

val propagate : Database.t -> Trail.t -> Pipeline.t -> Literal.t -> Conflict.t option

(* the following do not leak much implementation detail and are exposed for testing *)

val find_unit : (Literal.t -> Trail.option_bool) -> Literal.t array -> Literal.t option

val find_units
  :  Database.t
  -> (Literal.t -> Trail.option_bool)
  -> Literal.t
  -> (Literal.t * Clause_id.t) list
