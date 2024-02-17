open! Base
open Stdio

let () =
  let args = Sys.get_argv () in
  let filename = args.(1) in
  match Sat.Dimacs.read_file filename with
  | Ok result ->
    printf "number of variables: %d\n" (Sat.Dimacs.num_variables result);
    printf "clause %s" (Sat.Dimacs.clauses result).(0)
  | Error msg -> print_endline msg
;;
