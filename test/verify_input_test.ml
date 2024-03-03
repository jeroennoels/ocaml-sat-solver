open! Base
open! Stdio
open Sat
open Sat.Message

let assert_invalid_cnf lines expected_message =
  match Dimacs.read_lines lines with
  | Error msg ->
    print_endline (message_str "error reading dimacs" msg);
    false
  | Ok cnf ->
    (try
       Verify_input.verify cnf;
       false
     with
     | Verify_input.Invalid_cnf msg ->
       let success = String.equal msg expected_message in
       if not success then print_endline msg;
       success)
;;

let%test "trivial" =
  assert_invalid_cnf [ "p cnf 1 1"; "1 0" ] (message_int "trivial clause" 1)
;;

let%test "repeat [1 1]" =
  assert_invalid_cnf [ "p cnf 1 1"; "1 1 0" ] (message_int "clause repeats variable" 1)
;;

let%test "repeat [1 -1]" =
  assert_invalid_cnf [ "p cnf 1 1"; "1 -1 0" ] (message_int "clause repeats variable" 1)
;;

let%test "repeat [2 1 -2]" =
  assert_invalid_cnf [ "p cnf 2 1"; "2 1 -2 0" ] (message_int "clause repeats variable" 2)
;;

let%test "repeat [2 1 -1]" =
  assert_invalid_cnf [ "p cnf 2 1"; "2 1 -1 0" ] (message_int "clause repeats variable" 1)
;;

let%test "repeat [1 -2 3 4 2 -5]" =
  assert_invalid_cnf
    [ "p cnf 5 1"; "1 -2 3 4 2 -5 0" ]
    (message_int "clause repeats variable" 2)
;;

let%test "positive literal out of range" =
  assert_invalid_cnf
    [ "p cnf 5 1"; "1 2 3 4 5 6 0" ]
    (message_int "literal out of range" 6)
;;

let%test "negative literal out of range" =
  assert_invalid_cnf
    [ "p cnf 3 1"; "1 -4 3 -2 0" ]
    (message_int "literal out of range" (-4))
;;

let%test "positive missing literal" =
  assert_invalid_cnf
    [ "p cnf 5 3"; "1 2 4 5 0"; "-1 -2 -3 0"; "-4 -5 0" ]
    (message_int "missing literal" 3)
;;

let%test "negative missing literal" =
  assert_invalid_cnf
    [ "p cnf 4 3"; "1 2 0"; "-2 -3 0"; "-1 3 4 0" ]
    (message_int "missing literal" (-4))
;;
