open! Base
open Stdio
open Sat

let show a = Util.show_array ~f:Int.to_string a
let show_clause_ids = Util.show_array ~f:Clause_id.show
let show_clause = Util.show_array ~f:Literal.show

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
  let database = Database.create cnf in
  let nbvar = Cnf.num_variables cnf in
  let i = Clause_id.of_int 45 in
  print_endline "----------------------------";
  print_endline @@ show_clause (Database.get_literals database i);
  let x = Literal.of_int_check ~nbvar 58 in
  print_endline @@ show_clause_ids (Database.get_clause_ids database x)
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
