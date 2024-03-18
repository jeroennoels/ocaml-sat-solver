open! Base

type t = Conflict of Variable.t * Clause_id.t * Clause_id.t

let create (x, a1) (_, a2) = Conflict (Variable.of_literal x, a1, a2)
let variable (Conflict (v, _, _)) = v
let antecedent1 (Conflict (_, a, _)) = a
let antecedent2 (Conflict (_, _, a)) = a

let show (Conflict (v, a1, a2)) =
  Printf.sprintf
    "[Conflict(var=%s)(%s)(%s)]"
    (Variable.show v)
    (Clause_id.show a1)
    (Clause_id.show a2)
;;
