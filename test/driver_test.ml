open! Base
open! Stdio
open Sat

let%test "driver" =
  let cnf = Result.ok_or_failwith (Dimacs.read_lines Examples.factoring) in
  let database, trail, pipeline = Driver.initialize cnf in
  let conflict = Driver.run database trail pipeline in
  print_endline (Trail.show_assignment trail);
  if Option.is_some conflict then print_endline "conflict";
  true
;;
