open! Base
open! Stdio
open Sat

let make_database () =
  let clauses =
    [| [| 2; -3; 1 |]
     ; [| 6; 2 |]
     ; [| -2; 4; 5 |]
     ; [| -1; 2; -5 |]
     ; [| -3; -4 |]
     ; [| 1; 2; -3; -4; 5 |]
     ; [| 3; -6 |]
    |]
  in
  let cnf = Cnf.create 6 clauses in
  Verify_input.verify cnf;
  Database.create cnf
;;

let sample_literal x = Literal.of_int_check ~nbvar:6 x
let clause_id i = Clause_id.of_int i
let equal_int_arrays = Array.equal Int.equal
let int_array_sorted = Array.sorted_copy ~compare:Int.compare

let is_unboxed_int x =
  let r = Stdlib.Obj.repr x in
  Stdlib.Obj.is_int r && not (Stdlib.Obj.is_block r)
;;

let%test "unboxed literal" =
  let database = make_database () in
  let cid = clause_id 0 in
  let clause = Database.get_literals database cid in
  let x = clause.(0) in
  is_unboxed_int cid && Literal.to_int x = 2 && is_unboxed_int x
;;

let%test "unboxed clause_id" =
  let database = make_database () in
  let nbvar = Database.num_variables database in
  let x = Literal.of_int_check ~nbvar (-6) in
  let cids = Database.get_clause_ids database x in
  let cid = cids.(0) in
  is_unboxed_int x && Clause_id.to_int cid = 6 && is_unboxed_int cid
;;

let%test "get clause" =
  let database = make_database () in
  let clause = Database.get_literals database (clause_id 5) in
  equal_int_arrays [| 1; 2; -3; -4; 5 |] (Array.map ~f:Literal.to_int clause)
;;

let%test "relevant clauses" =
  let database = make_database () in
  let nbvar = Database.num_variables database in
  let run x =
    Literal.of_int_check ~nbvar x
    |> Database.get_clause_ids database
    |> Array.map ~f:Clause_id.to_int
    |> int_array_sorted
  in
  equal_int_arrays [| 0; 1; 3; 5 |] (run 2)
  && equal_int_arrays [| 2 |] (run (-2))
  && equal_int_arrays [| 0; 4; 5 |] (run (-3))
  && equal_int_arrays [| 2; 5 |] (run 5)
;;
