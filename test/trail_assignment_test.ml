open! Base
open! Stdio
open Sat

let print_assignment trail =
  printf
    "A%s U%s\n"
    (Trail.show_assignment trail)
    (Util.show_array ~f:Variable.show (Trail.copy_unassigned trail))
;;

let%expect_test "walk" =
  let nbvar = 8 in
  let trail = Trail.empty ~nbvar in
  let step i b =
    let x = Literal.of_int_check ~nbvar (if b then i else -i) in
    let cid = Clause_id.of_int 123 in
    Trail.step trail (x, cid)
  in
  let decide v b =
    let var = Variable.of_int_check ~nbvar v in
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

let%test_unit "walk randomly" =
  let nbvar = 100 in
  let trail = Trail.empty ~nbvar in
  (* zero-indexed array *)
  let last_assignment : bool array = Array.create ~len:nbvar false in
  let check_trail () =
    if not (Trail.invariant trail) then failwith "trail invariant violation";
    let good i b =
      let var = Variable.of_int_check ~nbvar (i + 1) in
      match Trail.eval_variable trail var with
      | True -> b
      | False -> not b
      | Undefined -> true
    in
    if not (Array.for_alli ~f:good last_assignment)
    then failwith "unexpected trail assignment"
  in
  for loop = 1 to nbvar do
    (* on every iteration, go one step further *)
    while Trail.length trail < loop do
      let var = Trail.random_unassigned_exn trail in
      let decision = Random.bool () in
      (* zero-indexed array *)
      let i = Variable.to_int var - 1 in
      last_assignment.(i) <- decision;
      Trail.decide trail var decision
    done;
    check_trail ();
    (* random backjump *)
    let len = Trail.length trail in
    let jump_to = Random.int_incl 0 len in
    Trail.backjump trail ~length:jump_to;
    check_trail ()
  done;
  check_trail ()
;;
