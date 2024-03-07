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

let () =
  let args = Sys.get_argv () in
  let filename = args.(1) in
  match Dimacs.read_file filename with
  | Ok cnf ->
    Verify_input.verify cnf;
    show_cnf_summary cnf
    (* let db = Database.create cnf in *)
    (* print_endline @@ show (Database.relevant_clauses db (Literal.of_int_check 58)) *)
  | Error msg -> print_endline msg
;;
