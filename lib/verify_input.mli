open! Base

exception Invalid_cnf of string

(* Raises Invalid_cnf exception when the input is invalid. *)
val verify : Cnf.t -> unit
