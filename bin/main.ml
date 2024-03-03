open! Base
open Stdio
open Sat

let () =
  let args = Sys.get_argv () in
  let filename = args.(1) in
  match Dimacs.read_file filename with
  | Ok result ->
    Verify_input.verify result;
    printf "Number of variables: %d\n" (Cnf_formula.num_variables result);
    printf "Number of clauses: %d\n" (Cnf_formula.num_clauses result);
    printf
      "Clauses:\n%s\n%s\n...\n%s\n"
      (Util.show (Cnf_formula.clauses result).(0))
      (Util.show (Cnf_formula.clauses result).(1))
      (Util.show (Cnf_formula.clauses result).(Cnf_formula.num_clauses result - 1))
  | Error msg -> print_endline msg
;;
