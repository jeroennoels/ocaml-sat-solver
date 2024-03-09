open! Base
open! Stdio
open Sat

let nbvar = 8
let literal = Literal.of_int_check nbvar
let var = Variable.of_int_check nbvar

let sample =
  let sample = Assignment.create ~nbvar in
  let assign i b = Assignment.assign sample (var i) b in
  let forget i = Assignment.forget sample (var i) in
  assign 1 false;
  assign 2 false;
  assign 4 false;
  forget 2;
  assign 2 true;
  assign 5 true;
  forget 4;
  assign 8 false;
  sample
;;

let%test "assign" = String.equal (Assignment.show sample) "(1:F)(2:T)(5:T)(8:F)"

let%expect_test "eval" =
  let run i =
    print_string
    @@
    match Assignment.eval sample (literal i) with
    | Assignment.Undefined -> "U"
    | Assignment.False -> "F"
    | Assignment.True -> "T"
  in
  for i = 1 to nbvar do
    run i;
    run (-i)
  done;
  [%expect {| FTTFUUUUTFUUUUFT |}]
;;
