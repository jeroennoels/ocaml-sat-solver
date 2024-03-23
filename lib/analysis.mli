open! Base

type t

val analyze_conflict : Database.t -> Trail.t -> Conflict.t -> t

(* for testing *)
val index : t -> Variable.t -> int option
