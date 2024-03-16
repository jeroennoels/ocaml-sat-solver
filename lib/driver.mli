open! Base

val initialize : Cnf.t -> Database.t * Trail.t * Pipeline.t
val run : Database.t -> Trail.t -> Pipeline.t -> Conflict.t option
