open! Base

type t = unit

let iter_reasons implied_var (xs : Literal.t array) ~(f : Variable.t -> unit) =
  let f x =
    let var = Variable.of_literal x in
    if Variable.equal var implied_var then () else f var
  in
  Array.iter ~f xs
;;

let trail_ends_at_conflict trail (conflict_reasons : Literal.t array) : bool =
  let last_step = Trail.last_step_exn trail in
  let f x = Variable.equal (Variable.of_literal x) last_step in
  Array.exists conflict_reasons ~f
;;

let analyze_conflict database trail conflict =
  let conflict_step = Trail.length trail in
  let decision_step = Trail.get_last_decision_step_exn trail in
  let num_steps = 1 + conflict_step - decision_step in
  let flow = Array.create ~len:num_steps 0 in
  let increase_flow delta var =
    let step = Trail.get_step trail var in
    let i = conflict_step - step in
    if i < num_steps then flow.(i) <- flow.(i) + delta else Stdio.printf "{%d}" i
  in
  let xs = Database.get_literals database (Conflict.antecedent1 conflict) in
  let ys = Database.get_literals database (Conflict.antecedent2 conflict) in
  let zs = Array.append xs ys in
  assert (trail_ends_at_conflict trail zs);
  let conflict_variable = Conflict.variable conflict in
  iter_reasons conflict_variable zs ~f:(increase_flow 12345)
;;
