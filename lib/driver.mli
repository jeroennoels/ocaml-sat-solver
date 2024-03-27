open! Base

val initialize : Cnf.t -> Database.t * Trail.t * Pipeline.t

val run
  :  Database.t
  -> Trail.t
  -> Pipeline.t
  -> (Literal.t * Clause_id.t) option
  -> Conflict.t option

val analyze : Database.t -> Trail.t -> Conflict.t option -> Analysis.t option

val drive
  :  Database.t
  -> Trail.t
  -> Pipeline.t
  -> (Literal.t * Clause_id.t) option
  -> Analysis.t * Clause_id.t

val solve : Database.t -> Trail.t -> Pipeline.t -> unit
