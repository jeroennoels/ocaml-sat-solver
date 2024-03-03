open! Base

type t =
  { num_variables : int
  ; clauses : int array array
  }

let create num_variables clauses = { num_variables; clauses }
let num_variables t = t.num_variables
let num_clauses t = Array.length t.clauses
let clauses t = t.clauses
