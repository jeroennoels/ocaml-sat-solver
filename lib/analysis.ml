open! Base

type t =
  { database : Database.t
  ; trail : Trail.t
  ; conflict_variable : Variable.t
  ; conflict_antecendent_dirty : Literal.t array
      (* may contain duplicates, and contains both a conflict literal and its negation *)
  ; conflict_step : int
  ; decision_step : int
  ; num_steps : int (* between last decision and conflict *)
  ; flow : int array
  }

let trail_ends_at_conflict (t : t) : bool =
  let var = Trail.last_step_variable_exn t.trail in
  let f x = Variable.equal (Variable.of_literal x) var in
  Array.exists t.conflict_antecendent_dirty ~f
  && not (Variable.equal var t.conflict_variable)
;;

let index (t : t) (var : Variable.t) : int option =
  let step = Trail.get_step t.trail var in
  let i = t.conflict_step - step in
  if i < t.num_steps then Some i else None
;;

let create database trail conflict =
  let conflict_variable = Conflict.variable conflict in
  let a1 = Database.get_literals database (Conflict.antecedent1 conflict) in
  let a2 = Database.get_literals database (Conflict.antecedent2 conflict) in
  let conflict_antecendent_dirty = Array.append a1 a2 in
  let conflict_step = Trail.length trail in
  let decision_step = Trail.get_last_decision_step_exn trail in
  let num_steps = 1 + conflict_step - decision_step in
  let flow = Array.create ~len:num_steps 0 in
  { database
  ; trail
  ; conflict_variable
  ; conflict_antecendent_dirty
  ; conflict_step
  ; decision_step
  ; num_steps
  ; flow
  }
;;

let analyze_conflict database trail conflict =
  let t = create database trail conflict in
  assert (trail_ends_at_conflict t);
  ignore (t.database, t.conflict_step, t.decision_step, t.flow, index);
  t
;;
