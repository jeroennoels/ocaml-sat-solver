open! Base
open! Stdio
open Sat

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
  match analysis with
  | None -> failwith "very unlikely to observe SAT here"
  | Some conflict_analysis ->
    let i = ref 0 in
    let f var _ =
      match Analysis.index conflict_analysis var with
      | Some j -> if j = !i then Int.incr i else failwith "wrong index"
      | None -> failwith "unexpected missing index"
    in
    Trail.iter_down_until_last_decision trail ~f;
    Cnf.num_conflicting counters = 0
;;
