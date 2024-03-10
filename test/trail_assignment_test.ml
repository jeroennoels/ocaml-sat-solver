open! Base
open! Stdio
open Sat

let%test "assign" =
  let nbvar = 8 in
  let cid_irrelevant = Clause_id.of_int 0 in
  let trail = Trail.empty ~nbvar in
  let literal i = Literal.of_int_check nbvar i in
  let assign i b =
    let x = literal (if b then i else -i) in
    Trail.step trail (x, cid_irrelevant)
  in
  assign 4 true;
  assign 1 false;
  assign 2 false;
  assign 8 true;
  assign 5 true;
  assign 7 false;
  let t1 = Trail.show_assignment trail in
  Trail.backjump trail ~length:4;
  let t2 = Trail.show_assignment trail in
  assign 3 true;
  assign 6 false;
  assign 7 false;
  assign 5 true;
  let t3 = Trail.show_assignment trail in
  Trail.backjump trail ~length:0;
  let t4 = Trail.show_assignment trail in
  String.equal t1 "(1:F)(2:F)(4:T)(5:T)(7:F)(8:T)"
  && String.equal t2 "(1:F)(2:F)(4:T)(8:T)"
  && String.equal t3 "(1:F)(2:F)(3:T)(4:T)(5:T)(6:F)(7:F)(8:T)"
  && String.equal t4 ""
;;
