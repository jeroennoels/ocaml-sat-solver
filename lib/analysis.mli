open! Base

type t

val analyze : Database.t -> Trail.t -> Conflict.t option -> t
