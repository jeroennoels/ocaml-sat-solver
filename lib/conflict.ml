open! Base

type t = Literal.t * Clause_id.t

let show (x, c) = Printf.sprintf "(%s,%s)" (Literal.show x) (Clause_id.show c)
