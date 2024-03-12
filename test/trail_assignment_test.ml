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
  let cid = Clause_id.of_int 123 in
  let trail = Trail.empty ~nbvar in
  let step i b =
    let x = Literal.of_int_check nbvar (if b then i else -i) in
    Trail.step trail (x, cid)
  in
  let decide v b =
    let var = Variable.of_int_check nbvar v in
    Trail.decide trail var b
  in
  decide 4 true;
  step 1 false;
  step 2 false;
  print_assignment trail;
  step 8 true;
  step 5 true;
  decide 7 false;
  print_assignment trail;
  Trail.backjump trail ~length:4;
  print_assignment trail;
  decide 3 true;
  step 6 false;
  step 7 false;
  step 5 true;
  print_assignment trail;
  Trail.backjump trail ~length:0;
  print_assignment trail;
  step 6 false;
  step 2 false;
  step 5 true;
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
