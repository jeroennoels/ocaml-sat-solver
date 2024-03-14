open! Base
open! Stdio
open Sat

let literal i = Literal.of_int_check ~nbvar:1000 i
let antecedent i = literal i, Clause_id.of_int 123

let%test "pipeline" =
  let pipeline = Pipeline.empty () in
  assert (Option.is_none (Pipeline.dequeue pipeline));
  let result =
    Pipeline.enqueue_all pipeline [ antecedent 5; antecedent 2; antecedent (-3) ]
  in
  Option.is_none result
;;
