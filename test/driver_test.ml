open! Base
open! Stdio
open Sat

let%test "driver" =
  let cnf = Result.ok_or_failwith (Dimacs.read_lines Examples.factoring) in
  let database, trail, pipeline = Driver.initialize cnf in
  Trail.set_logging trail true;
  Pipeline.set_logging pipeline false;
  ignore @@ Driver.run database trail pipeline;
  Trail.set_logging trail false;
  Pipeline.set_logging pipeline false;
  print_endline (Trail.show_assignment trail);
  let eval = Trail.eval_literal_nodeps trail in
  let counters = Cnf.evaluate cnf eval in
  print_endline (Cnf.show_counters counters);
  Trail.iter_down_until_last_decision trail ~f:(fun var ->
    printf "{%s}" (Variable.show var));
  print_endline "";
  Cnf.num_conflicting counters = 0
;;
