open! Base

type t =
  { database : Database.t
  ; trail : Trail.t
  ; conflict_variable : Variable.t
  ; conflict_antecendent_dirty : Literal.t array
      (* dirty: may contain duplicates and includes the conflict pair *)
  ; conflict_step : int
  ; decision_step : int
  ; num_steps : int (* between last decision and conflict *)
  ; flow : int array
  ; mutable learned_clause : (int, Int.comparator_witness) Set.t
  }

let get_conflict_variable (t : t) = t.conflict_variable
let get_num_steps (t : t) = t.num_steps

let print (t : t) =
  Stdio.printf
    "conflict_variable = %s\nnum_steps = %d\nlearned_clause = [%s]\n"
    (Variable.show t.conflict_variable)
    t.num_steps
    (Util.show_list (Set.to_list t.learned_clause) ~f:Int.to_string)
;;

let add_to_learned_clause (t : t) (x : Literal.t) =
  t.learned_clause <- Set.add t.learned_clause (Literal.to_int x)
;;

let trail_ends_at_conflict (t : t) : bool =
  let var = Trail.last_step_variable_exn t.trail in
  let f x = Variable.equal (Variable.of_literal x) var in
  Array.exists t.conflict_antecendent_dirty ~f
  && not (Variable.equal var t.conflict_variable)
;;

let index (t : t) (var : Variable.t) : int option =
  let step = Trail.get_step t.trail var in
  let i = t.conflict_step - step in
  assert (i >= 0);
  if i < t.num_steps then Some i else None
;;

let partition (t : t) implied_var (xs : Literal.t array) : int list =
  let (parents : int list ref) = ref [] in
  let f x =
    let var = Variable.of_literal x in
    if not (Variable.equal var implied_var)
    then (
      match index t var with
      | Some i -> parents := i :: !parents
      | None -> add_to_learned_clause t x)
  in
  Array.iter ~f xs;
  !parents
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
  ; learned_clause = Set.empty (module Int)
  }
;;

let analyze_conflict database trail conflict =
  let t = create database trail conflict in
  assert (trail_ends_at_conflict t);
  ignore (t.database, t.decision_step, t.flow, partition);
  t
;;
