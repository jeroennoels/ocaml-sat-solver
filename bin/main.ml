open! Base
open Stdio
open Sat

let () =
  let args = Sys.get_argv () in
  let filename = args.(1) in
  match Dimacs.read_file filename with
  | Ok result ->
    Verify_input.verify result;
    printf "Number of variables: %d\n" (Dimacs.num_variables result);
    printf "Number of clauses: %d\n" (Dimacs.num_clauses result);
    printf
      "Clauses:\n%s\n%s\n...\n%s\n"
      (Util.show (Dimacs.clauses result).(0))
      (Util.show (Dimacs.clauses result).(1))
      (Util.show (Dimacs.clauses result).(Dimacs.num_clauses result - 1))
  | Error msg -> print_endline msg
;;
