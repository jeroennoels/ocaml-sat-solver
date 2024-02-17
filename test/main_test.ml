open! Base
open Stdio
open Sat

let%test "words" =
  let eq = List.equal String.equal in
  eq (Util.words "p cnf   25\n \t66 ") [ "p"; "cnf"; "25"; "66" ]
  && eq (Util.words "\t-51 9 -243 88  0\t") [ "-51"; "9"; "-243"; "88"; "0" ]
;;

let%test "startsWith" =
  let p str = Util.startsWith str 'a' in
  p "abc" && p "a" && (not (p "")) && not (p "xyz")
;;

let failWithMessage x y =
  print_endline (x ^ " " ^ y);
  false
;;

let dimacs_expect_ok lines (verifier : Dimacs.t -> bool) =
  match Dimacs.read_lines lines with
  | Ok p -> verifier p
  | Error msg -> failWithMessage "error:" msg
;;

let dimacs_expect_error lines expected_msg =
  match Dimacs.read_lines lines with
  | Ok _ -> failWithMessage "undetected error case, expecting:" expected_msg
  | Error msg ->
    if String.equal msg expected_msg
    then true
    else failWithMessage "wrong error message:" msg
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
  in
  dimacs_expect_ok cnf (fun formula ->
    Dimacs.num_clauses formula = 3
    && Dimacs.num_variables formula = 5
    && String.equal (Dimacs.clauses formula).(2) "-4 -3 0")
;;

let%test "dimacs error: missing p line" =
  let cnf = [ "1 -2 4 0" ] in
  dimacs_expect_error cnf "missing problem line"
;;

let%test "dimacs error: not enough clauses" =
  let cnf = [ "p cnf 5 3"; "1 -2 4 0"; "5 -3 2 0" ] in
  dimacs_expect_error cnf "not enough clauses"
;;

let%test "dimacs error: too many clauses" =
  let cnf = [ "p cnf 3 2"; "1 2 0"; "1 -3 0"; "-2 3 0" ] in
  dimacs_expect_error cnf "too many clauses"
;;
