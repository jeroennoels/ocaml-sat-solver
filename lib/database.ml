open! Base

type literal = Literal.t
type clause = literal array
type t = { clauses : clause array }

let get_clause t i = t.clauses.(Clause_id.to_int i)
let relevant_clauses _ _ = Array.create ~len:1 (Clause_id.of_int_check 0)

let typeful_clauses cnf : clause array =
  let nbvar = Cnf.num_variables cnf in
  let typeful x = Literal.of_int_check nbvar x in
  Array.map ~f:(Array.map ~f:typeful) (Cnf.clauses cnf)
;;

let create cnf = { clauses = typeful_clauses cnf }
