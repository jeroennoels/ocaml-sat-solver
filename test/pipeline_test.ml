open! Base
open! Stdio
open Sat

let literal i = Literal.of_int_unchecked i

(* dealing with the clause is for later *)
let antecedent i = literal i, Clause_id.of_int 123

let enqueue pipeline (xs : int list) =
  List.map ~f:antecedent xs |> Pipeline.enqueue_all pipeline
;;

let enqueue_no_conflict pipeline xs : unit =
  let conflict = enqueue pipeline xs in
  if Option.is_some conflict then failwith "unexpected conflict" else ()
;;

let%test "empty" = Option.is_none (Pipeline.dequeue (Pipeline.create ()))

let print_dequeue pipeline =
  match Pipeline.dequeue pipeline with
  | Some (x, _) -> printf "(%s)" (Literal.show x)
  | None -> print_endline "EMPTY"
;;

let%expect_test "dequeue" =
  let pipeline = Pipeline.create () in
  enqueue_no_conflict pipeline [ 2; -6 ];
  enqueue_no_conflict pipeline [ 3; -5; 8 ];
  for _ = 1 to 6 do
    print_dequeue pipeline
  done;
  [%expect {| (2)(-6)(3)(-5)(8)EMPTY |}]
;;

let%expect_test "duplicates" =
  let pipeline = Pipeline.create () in
  enqueue_no_conflict pipeline [ 2; 6; 7; 2 ];
  enqueue_no_conflict pipeline [ 3; -5; 7; 7 ];
  enqueue_no_conflict pipeline [ 2; 6; -1 ];
  for _ = 1 to 7 do
    print_dequeue pipeline
  done;
  enqueue_no_conflict pipeline [ 3; 7; -5; 7 ];
  for _ = 1 to 4 do
    print_dequeue pipeline
  done;
  [%expect {|
    (2)(6)(7)(3)(-5)(-1)EMPTY
    (3)(7)(-5)EMPTY
    |}]
;;

let%expect_test "conflict" =
  let pipeline = Pipeline.create () in
  enqueue_no_conflict pipeline [ 2; 6; 7; 2 ];
  enqueue_no_conflict pipeline [ -3; 6; -4; 2 ];
  let conflict = enqueue pipeline [ 1; 2; 4; 6; -7 ] in
  (match conflict with
   | Some ((x, _), (y, _)) -> printf "(%d)(%d)" (Literal.to_int x) (Literal.to_int y)
   | _ -> printf "no-conflict");
  [%expect {| (4)(-4) |}]
;;
