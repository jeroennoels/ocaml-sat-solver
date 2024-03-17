open! Base

type t = (Literal.t * Clause_id.t) * (Literal.t * Clause_id.t)

let show (a, b) =
  let show (x, c) = Printf.sprintf "(%s,%s)" (Literal.show x) (Clause_id.show c) in
  Printf.sprintf "Conflict of %s with %s" (show a) (show b)
;;
