open! Base

val initialize : Cnf.t -> Database.t * Trail.t * Pipeline.t
val run : Database.t -> Trail.t -> Pipeline.t -> Conflict.t option
val analyze : Database.t -> Trail.t -> Conflict.t option -> Analysis.t option
