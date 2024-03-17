open! Base

type option_bool =
  | Undefined
  | False
  | True

type t

val negate : option_bool -> option_bool
val create : nbvar:int -> t
val is_complete : t -> bool
val decide : t -> Variable.t -> bool -> Literal.t
val step : t -> Literal.t * Clause_id.t -> unit
val backjump : t -> length:int -> unit
val eval_variable : t -> Variable.t -> option_bool
val eval_literal : t -> Literal.t -> option_bool
val is_assigned : t -> Literal.t -> bool
val random_unassigned_exn : t -> Variable.t
val decision_level_exn : t -> Variable.t -> int

(* only for debugging, logging and testing *)
val show_assignment : t -> string
val copy_unassigned : t -> Variable.t array
val invariant : t -> bool
val length : t -> int
val set_logging : t -> bool -> unit
val eval_literal_nodeps : t -> int -> bool option
