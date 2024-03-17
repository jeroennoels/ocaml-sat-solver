open! Base

type t = (Literal.t * Clause_id.t) * (Literal.t * Clause_id.t)

let show (a, b) =
  let show_antecedent (x, c) = "(" ^ Literal.show x ^ "," ^ Clause_id.show c ^ ")" in
  "Conflict of " ^ show_antecedent a ^ " with " ^ show_antecedent b
;;
