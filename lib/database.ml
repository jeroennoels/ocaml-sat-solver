open! Base

(* type literal = Literal.t *)
(* type clause_id = Clause_id.t *)
(* type clause = literal array *)
type t = unit
(* { negative : clause_id array array *)
(* ; positive : clause_id array array *)
(* ; clauses : clause array *)
(* } *)

let relevant_clauses _ _ = failwith "todo"
(* if x > 0 then t.negative.(x) else t.positive.(-x) *)

(* let create_clause_array _ = () *)
let create _ = failwith "todo"
