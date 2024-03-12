open! Base

type option_bool =
  | Undefined
  | False
  | True

type t

val negate : option_bool -> option_bool
val empty : nbvar:int -> t
val decide : t -> Variable.t -> bool -> unit
val step : t -> Literal.t * Clause_id.t -> unit
val backjump : t -> length:int -> unit
val eval_variable : t -> Variable.t -> option_bool
val eval_literal : t -> Literal.t -> option_bool
val random_unassigned_exn : t -> Variable.t
val decision_level_exn : t -> Variable.t -> int

(* only for debugging and testing *)
val show_assignment : t -> string
val copy_unassigned : t -> Variable.t array
val invariant : t -> bool
