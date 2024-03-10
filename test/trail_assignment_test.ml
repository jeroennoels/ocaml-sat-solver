open! Base
open! Stdio
open Sat

let print_assignment trail =
  printf
    "A%s U%s\n"
    (Trail.show_assignment trail)
    (Util.show_array ~f:Variable.show (Trail.copy_unassigned trail))
;;

let%expect_test "assign" =
  let nbvar = 8 in
  let cid = Clause_id.of_int 0 in
  let trail = Trail.empty ~nbvar in
  let literal i = Literal.of_int_check nbvar i in
  let assign i b =
    let x = literal (if b then i else -i) in
    Trail.step trail (x, cid)
  in
  assign 4 true;
  assign 1 false;
  assign 2 false;
  print_assignment trail;
  assign 8 true;
  assign 5 true;
  assign 7 false;
  print_assignment trail;
  Trail.backjump trail ~length:4;
  print_assignment trail;
  assign 3 true;
  assign 6 false;
  assign 7 false;
  assign 5 true;
  print_assignment trail;
  Trail.backjump trail ~length:0;
  print_assignment trail;
  assign 6 false;
  assign 2 false;
  assign 5 true;
  print_assignment trail;
  [%expect
    {|
A(1:F)(2:F)(4:T) U3 5 6 7 8
A(1:F)(2:F)(4:T)(5:T)(7:F)(8:T) U3 6
A(1:F)(2:F)(4:T)(8:T) U3 5 6 7
A(1:F)(2:F)(3:T)(4:T)(5:T)(6:F)(7:F)(8:T) U
A U1 2 3 4 5 6 7 8
A(2:F)(5:T)(6:F) U1 3 4 7 8
 |}]
;;
