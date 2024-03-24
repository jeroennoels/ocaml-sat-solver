open! Base
open! Stdio
open Sat

let verify trail (analysis : Analysis.t) : bool =
  Analysis.print analysis;
  let check_index i var _ =
    match Analysis.index analysis var with
    | Some j -> if not (j = i) then failwith "wrong index"
    | None -> failwith "unexpected missing index"
  in
  let conflict_variable = Analysis.get_conflict_variable analysis in
  let choose_literal var = Variable.to_literal var true in
  let xs = Array.map (Trail.copy_assigned trail) ~f:choose_literal in
  let conflict_parents = Analysis.partition analysis conflict_variable xs in
  let num_steps = Analysis.get_num_steps analysis in
  Trail.iteri_down_until_last_decision trail ~f:check_index;
  List.equal Int.equal conflict_parents (List.range 0 ~stop:`exclusive num_steps)
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
  | Some conflict_analysis -> verify trail conflict_analysis
;;
