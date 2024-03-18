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
  let eval = Trail.eval_literal_nodeps trail in
  let counters = Cnf.evaluate cnf eval in
  print_endline (Cnf.show_counters counters);
  let kappa_is_conflicting =
    match conflict with
    | Some details ->
      let kappa = Database.get_literals database (Conflict.get_kappa details) in
      Cnf.conflicting eval (Array.map ~f:Literal.to_int kappa)
    | None -> false
  in
  Cnf.num_conflicting counters = 1 && kappa_is_conflicting
;;
