open! Base
open! Stdio
open Sat

let%test "driver" =
  let cnf = Result.ok_or_failwith (Dimacs.read_lines Examples.factoring) in
  let database, trail, pipeline = Driver.initialize cnf in
  Trail.set_logging trail true;
  Pipeline.set_logging pipeline false;
  let conflict = Driver.run database trail pipeline in
  Trail.set_logging trail false;
  Pipeline.set_logging pipeline false;
  print_endline (Trail.show_assignment trail);
  let counters = Cnf.evaluate cnf (Trail.eval_literal_nodeps trail) in
  print_endline (Cnf.show_counters counters);
  if Option.is_some conflict then print_endline "conflict";
  true
;;
