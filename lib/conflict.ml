open! Base

type t = Conflict of (Literal.t * Clause_id.t) * Clause_id.t

let create step kappa = Conflict (step, kappa)
let get_step (Conflict (step, _)) = step
let get_kappa (Conflict (_, kappa)) = kappa

let show (Conflict ((x, c), kappa)) =
  Printf.sprintf
    "[step=(%s,%s);kappa=%s]"
    (Literal.show x)
    (Clause_id.show c)
    (Clause_id.show kappa)
;;
