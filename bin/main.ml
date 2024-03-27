open! Base
open Stdio
open Sat

let show a = Util.show_array ~f:Int.to_string a

let show_cnf_summary cnf =
  printf "Number of variables: %d\n" (Cnf.num_variables cnf);
  printf "Number of clauses: %d\n" (Cnf.num_clauses cnf);
  let last = Cnf.num_clauses cnf - 1 in
  printf
    "----------------------------\n%s\n%s\n...\n%s\n"
    (show (Cnf.clauses cnf).(0))
    (show (Cnf.clauses cnf).(1))
    (show (Cnf.clauses cnf).(last))
;;

let run cnf =
  show_cnf_summary cnf;
  print_endline "----------------------------";
  let database, trail, pipeline = Driver.initialize cnf in
  Trail.set_logging trail true;
  let analysis = Driver.drive database trail pipeline in
  Trail.set_logging trail false;
  print_endline (Trail.show_assignment trail);
  print_endline "----------------------------";
  let counters = Cnf.evaluate cnf (Trail.eval_literal_nodeps trail) in
  print_endline (Cnf.show_counters counters);
  Analysis.print analysis
;;

let () =
  let args = Sys.get_argv () in
  let filename = args.(1) in
  match Dimacs.read_file filename with
  | Ok cnf ->
    Verify_input.verify cnf;
    run cnf
  | Error msg -> print_endline msg
;;
