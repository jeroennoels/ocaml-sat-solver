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
  let f units xs cid =
    match find_unit eval xs with
    | Some y -> (y, cid) :: units
    | None -> units
  in
  Database.fold_over_clauses_containing database ~f (Literal.negate x)
;;

let propagate database trail pipeline kickoff =
  (* recycle allocated space *)
  assert (Pipeline.is_empty pipeline);
  assert (Trail.is_assigned trail kickoff);
  let rec go x =
    let units = find_units database (Trail.eval_literal trail) x in
    match Pipeline.enqueue_all pipeline units with
    | None ->
      (match Pipeline.dequeue pipeline with
       | Some ((y, _) as antecedent) ->
         Trail.step trail antecedent;
         go y
       | None -> None)
    | conflict ->
      Pipeline.clear pipeline;
      conflict
  in
  go kickoff
;;
