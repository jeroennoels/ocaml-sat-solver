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
    printf "Clause: %s\n" (show (Dimacs.clauses result).(0))
  | Error msg -> print_endline msg
;;
