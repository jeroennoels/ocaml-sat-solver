open! Base
open Stdio
open Sat
open Sat.Message

let fail_with_message text arg =
  print_endline (message_str text arg);
  false
;;

let expect_ok lines (verifier : Cnf_formula.t -> bool) =
  match Dimacs.read_lines lines with
  | Ok cnf -> verifier cnf
  | Error msg -> fail_with_message "error" msg
;;

let expect_error lines expected_msg =
  match Dimacs.read_lines lines with
  | Ok _ -> fail_with_message "success while expecting error" expected_msg
  | Error msg ->
    if String.equal msg expected_msg
    then true
    else fail_with_message "wrong error message:" msg
;;

let%test "dimacs ok" =
  let cnf =
    [ "c this is a comment"
    ; "c more comments"
    ; "p cnf 5 3"
    ; "1 -2 4 0"
    ; "5 -3 2 0"
    ; "-4 -3 0"
    ]
  and verifier formula =
    Cnf_formula.num_clauses formula = 3
    && Cnf_formula.num_variables formula = 5
    && Array.equal
         (Array.equal Int.equal)
         (Cnf_formula.clauses formula)
         [| [| 1; -2; 4 |]; [| 5; -3; 2 |]; [| -4; -3 |] |]
  in
  expect_ok cnf verifier
;;

let%test "missing p line" =
  let cnf = [ "c"; "1 -2 4 0" ] in
  expect_error cnf "missing p line"
;;

let%test "not enough clauses" =
  let cnf = [ "p cnf 5 3"; "1 -2 4 0"; "5 -3 2 0" ] in
  expect_error cnf "not enough clauses"
;;

let%test "too many clauses" =
  let cnf = [ "p cnf 3 2"; "1 2 0"; "1 -3 0"; "-2 3 0" ] in
  expect_error cnf "too many clauses"
;;

let%test "non terminated clause" =
  let cnf = [ "p cnf 3 2"; "1 2 0"; "1 -3 2" ] in
  expect_error cnf (message_str "invalid clause line" "1 -3 2")
;;

let%test "empty clause" =
  let cnf = [ "p cnf 3 2"; "1 2 0"; "0" ] in
  expect_error cnf (message_str "invalid clause line" "0")
;;
