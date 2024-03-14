open! Base
open! Stdio
open Sat

let literal i = Literal.of_int_check ~nbvar:1000 i

(* dealing with the clause is for later *)
let antecedent i = literal i, Clause_id.of_int 123

let enqueue pipeline (xs : int list) =
  List.map ~f:antecedent xs |> Pipeline.enqueue_all pipeline
;;

let enqueue_no_conflict pipeline xs : unit =
  let conflict = enqueue pipeline xs in
  if Option.is_some conflict then failwith "unexpected conflict" else ()
;;

let%test "empty" = Option.is_none (Pipeline.dequeue (Pipeline.empty ()))

let print_dequeue pipeline =
  match Pipeline.dequeue pipeline with
  | Some (x, _) -> printf "(%s)" (Literal.show x)
  | None -> print_endline "EMPTY"
;;

let%expect_test "dequeue" =
  let pipeline = Pipeline.empty () in
  enqueue_no_conflict pipeline [ 2; -6 ];
  enqueue_no_conflict pipeline [ 3; -5; 8 ];
  for _ = 1 to 6 do
    print_dequeue pipeline
  done;
  [%expect {| (2)(-6)(3)(-5)(8)EMPTY |}]
;;

let%test "pipeline" =
  let pipeline = Pipeline.empty () in
  enqueue_no_conflict pipeline [ 5; 2; -3 ];
  Option.is_none (enqueue pipeline [ 7; 6 ])
;;
