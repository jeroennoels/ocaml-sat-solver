open! Base
open Stdio
open Sat

let show clause =
  String.concat ~sep:" " (List.map ~f:Int.to_string (Array.to_list clause))
;;

let () =
  let args = Sys.get_argv () in
  let filename = args.(1) in
  match Dimacs.read_file filename with
  | Ok result ->
    printf "Number of variables: %d\n" (Dimacs.num_variables result);
    printf "Number of clauses: %d\n" (Dimacs.num_clauses result);
    printf
      "Clauses:\n%s\n%s\n...\n%s\n"
      (show (Dimacs.clauses result).(0))
      (show (Dimacs.clauses result).(1))
      (show (Dimacs.clauses result).(Dimacs.num_clauses result - 1))
  | Error msg -> print_endline msg
;;
