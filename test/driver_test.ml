open! Base
open! Stdio
open Sat

let log = false

let verify_learned_clause
  database
  (learned : Literal.t array)
  (uip : Literal.t)
  (conflict_variable : Variable.t)
  : bool
  =
  let nbvar = Database.num_variables database in
  let trail = Trail.create ~nbvar in
  let pipeline = Pipeline.create () in
  (* build a fresh trail based on the learned clause *)
  let decide y =
    let z = Trail.decide trail (Variable.of_literal y) (Literal.is_positive y) in
    assert (Literal.equal y z)
  in
  (* use negation for non-UIP literals *)
  let f x = if not (Literal.same_variable x uip) then decide (Literal.negate x) in
  Array.iter learned ~f;
  decide uip;
  (* replay all implications starting from the UIP and expect the same conflict *)
  match Propagation.propagate database trail pipeline uip with
  | None -> false
  | Some conflict -> Variable.equal conflict_variable (Conflict.variable conflict)
;;

let verify database (analysis : Analysis.t) : bool =
  if log then Analysis.print analysis;
  let learned = Analysis.get_learned_clause_exn analysis in
  let conflict_variable = Analysis.get_conflict_variable analysis in
  match Analysis.get_uip_literal analysis with
  | None -> false
  | Some uip -> verify_learned_clause database learned uip conflict_variable
;;

let run_once () =
  let cnf = Result.ok_or_failwith (Dimacs.read_lines Examples.factoring) in
  let database, trail, pipeline = Driver.initialize cnf in
  Trail.set_logging trail log;
  let result = Driver.run database trail pipeline None in
  let analysis = Driver.analyze database trail result in
  let eval = Trail.eval_literal_nodeps trail in
  let counters = Cnf.evaluate cnf eval in
  if log then print_endline (Cnf.show_counters counters);
  Cnf.num_conflicting counters = 0
  &&
  (* we could hit SAT by accident, but that is not the purpose of this test *)
  match analysis with
  | None -> failwith "very unlikely to observe SAT here"
  | Some conflict_analysis -> verify database conflict_analysis
;;

let%test "driver" =
  let repetitions = 10 in
  List.for_all ~f:run_once (List.init repetitions ~f:(Fn.const ()))
;;
