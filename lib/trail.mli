open! Base

type option_bool =
  | Undefined
  | False
  | True

type t

val negate : option_bool -> option_bool
val create : nbvar:int -> t
val num_variables : t -> int
val length : t -> int
val is_complete : t -> bool
val decide : t -> Variable.t -> bool -> Literal.t
val step : t -> Literal.t * Clause_id.t -> unit
val get_step : t -> Variable.t -> int
val backjump : t -> length:int -> unit
val eval_variable : t -> Variable.t -> option_bool
val eval_literal : t -> Literal.t -> option_bool
val is_assigned : t -> Literal.t -> bool
val random_unassigned_exn : t -> Variable.t
val get_last_decision_step_exn : t -> int

val iteri_down_until_last_decision
  :  t
  -> f:(int -> Variable.t -> Clause_id.t -> unit)
  -> unit

(* only for debugging, asserting, logging and testing *)
val last_step_variable_exn : t -> Variable.t
val show_assignment : t -> string
val copy_unassigned : t -> Variable.t array
val copy_assigned : t -> Variable.t array
val invariant : t -> bool
val set_logging : t -> bool -> unit
val eval_literal_nodeps : t -> int -> bool option
