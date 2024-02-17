open! Base
open Stdio
open Sat

let () =
  let args = Sys.get_argv () in
  let filename = args.(1) in
  match Dimacs.read_file filename with
  | Ok result ->
    printf "Number of variables: %d\n" (Dimacs.num_variables result);
    printf "Clause: %s\n" (Dimacs.clauses result).(0)
  | Error msg -> print_endline msg
;;
