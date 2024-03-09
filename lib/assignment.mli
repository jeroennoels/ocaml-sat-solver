open! Base

type option_bool =
  | Undefined
  | False
  | True

type t

val negate : option_bool -> option_bool
val create : nbvar:int -> t
val eval : t -> Literal.t -> option_bool
val assign : t -> Variable.t -> bool -> unit
val forget : t -> Variable.t -> unit
val show : t -> string
