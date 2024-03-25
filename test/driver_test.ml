open! Base
open! Stdio
open Sat

let verify_learned_clause
  database
  (learned : Literal.t array)
  (uip : Literal.t)
  (conflict_variable : Variable.t)
  : bool
  =
  let nbvar = Database.num_variables database in
  let trail = Trail.create ~nbvar in
  Trail.set_logging trail true;
  let pipeline = Pipeline.create () in
  let decide y =
    let z = Trail.decide trail (Variable.of_literal y) (Literal.is_positive y) in
    assert (Literal.equal y z)
  in
  let f x = if not (Literal.same_variable x uip) then decide (Literal.negate x) in
  Array.iter learned ~f;
  decide uip;
  match Propagation.propagate database trail pipeline uip with
  | None -> false
  | Some conflict -> Variable.equal conflict_variable (Conflict.variable conflict)
;;

let verify database (analysis : Analysis.t) : bool =
  Analysis.print analysis;
  let learned = Analysis.get_learned_clause_exn analysis in
  let conflict_variable = Analysis.get_conflict_variable analysis in
  match Analysis.get_uip_literal analysis with
  | None -> false
  | Some uip -> verify_learned_clause database learned uip conflict_variable
;;

let%test "driver" =
  let cnf = Result.ok_or_failwith (Dimacs.read_lines Examples.factoring) in
  let database, trail, pipeline = Driver.initialize cnf in
  Trail.set_logging trail true;
  Pipeline.set_logging pipeline false;
  let result = Driver.run database trail pipeline in
  let analysis = Driver.analyze database trail result in
  Trail.set_logging trail false;
  Pipeline.set_logging pipeline false;
  print_endline (Trail.show_assignment trail);
  let eval = Trail.eval_literal_nodeps trail in
  let counters = Cnf.evaluate cnf eval in
  print_endline (Cnf.show_counters counters);
  Cnf.num_conflicting counters = 0
  &&
  match analysis with
  | None -> failwith "very unlikely to observe SAT here"
  | Some conflict_analysis -> verify database conflict_analysis
;;
