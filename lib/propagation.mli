open! Base

val find_unit
  :  (Literal.t -> Assignment.option_bool)
  -> Literal.t array
  -> Literal.t option

val find_units
  :  Database.t
  -> (Literal.t -> Assignment.option_bool)
  -> Literal.t
  -> (Literal.t * Clause_id.t) list