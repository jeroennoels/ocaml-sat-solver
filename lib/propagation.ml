open! Base

type literal = Literal.t
type clause_id = Clause_id.t
type option_bool = Trail.option_bool

let find_unit (eval : literal -> option_bool) (clause : literal array) =
  let rec go i candidate =
    if i < 0
    then candidate
    else (
      let x = clause.(i) in
      match eval x with
      | True -> None
      | False -> go (i - 1) candidate
      | Undefined ->
        (match candidate with
         | None -> go (i - 1) (Some x)
         | Some _ -> None))
  in
  go (Array.length clause - 1) None
;;

let find_units database (eval : literal -> option_bool) (x : literal)
  : (literal * clause_id) list
  =
  let relevant_clauses = Database.get_clause_ids database (Literal.negate x) in
  let f units cid =
    let xs = Database.get_literals database cid in
    match find_unit eval xs with
    | Some y -> (y, cid) :: units
    | None -> units
  in
  Array.fold ~init:[] ~f relevant_clauses
;;

let propagate _ _ _ _ = None
