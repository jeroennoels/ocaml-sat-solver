open! Base
open! Stdio
open Sat

let literal = Literal.of_int_check 1000
let literals = Array.map ~f:literal

let assignment v =
  match Variable.to_int v with
  | 1 | 3 | 5 -> Assignment.True
  | 2 | 4 | 6 -> Assignment.False
  | _ -> Assignment.Undefined
;;

let eval x =
  let value = assignment (Variable.of_literal x) in
  if Literal.is_positive x then value else Assignment.negate value
;;

let shuffle a =
  let copy = Array.copy a in
  Array.permute copy;
  copy
;;

let test_find_unit ~randomize =
  let samples =
    [ [| 2; 9 |], Some 9
    ; [| 1; 2 |], None
    ; [| -3; -4 |], None
    ; [| 7; 8 |], None
    ; [| 7; -8; 2 |], None
    ; [| 4; 6; 8; 9 |], None
    ; [| 2; 4; 6; 8 |], Some 8
    ; [| -1; -3; -5; -9 |], Some (-9)
    ]
  in
  let test (clause, expected) =
    let xs = literals clause in
    let ys = if randomize then shuffle xs else xs in
    match expected, Propagation.find_unit eval ys with
    | Some i, Some x -> i = Literal.to_int x
    | None, None -> true
    | _ -> false
  in
  List.for_all ~f:test samples
;;

let%test "find unit" = test_find_unit ~randomize:false

let%test "find unit randomized" =
  List.for_all
    ~f:(fun randomize -> test_find_unit ~randomize)
    [ true; true; true; true; true; true; true; true ]
;;

let show_unit = Util.show_pair ~f:Literal.show ~g:Clause_id.show

let%expect_test "find units" =
  let lines =
    [ "p cnf 9 5"
    ; "2 4 6 8 0"
    ; "-1 -3 -5 4 9 0"
    ; "1 3 -4 5 7 8 9 0"
    ; "-2 4 5 -6 -7 -8 -9 0"
    ; "4 8 0"
    ]
  in
  let cnf = Result.ok_or_failwith (Dimacs.read_lines lines) in
  Verify_input.verify cnf;
  let units = Propagation.find_units (Database.create cnf) eval (literal (-4)) in
  print_endline @@ String.concat ~sep:" " (List.map ~f:show_unit units);
  [%expect {| (8, 0) (9, 1) (8, 4) |}]
;;
