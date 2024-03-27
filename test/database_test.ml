open! Base
open! Stdio
open Sat

let literal i = Literal.of_int_unchecked i
let literals ints = Array.map ~f:literal ints
let clause_id i = Clause_id.of_int i
let equal_int_arrays = Array.equal Int.equal
let int_array_sorted = Array.sorted_copy ~compare:Int.compare

let make_database () =
  let clauses =
    [| [| 2; -3; 1 |]
     ; [| 6; 2 |]
     ; [| -2; 4; 5 |]
     ; [| -1; 2; -5 |]
     ; [| 1; 2; -3; -4; 5 |]
     ; [| 3; -6 |]
    |]
  in
  let cnf = Cnf.create 6 clauses in
  Verify_input.verify cnf;
  let database = Database.create cnf in
  let learn ints = Database.add_learned_clause database (literals ints) in
  learn [| -2; -3; -4 |];
  learn [| 1; 2; -5; 6 |];
  learn [| -1; 6 |];
  learn [| -2; 5 |];
  database
;;

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
  is_unboxed_int x && Clause_id.to_int cid = 5 && is_unboxed_int cid
;;

let%test "get clause" =
  let database = make_database () in
  let clause = Database.get_literals database (clause_id 4) in
  equal_int_arrays [| 1; 2; -3; -4; 5 |] (Array.map ~f:Literal.to_int clause)
;;

let%test "get learned clause" =
  let database = make_database () in
  let clause = Database.get_literals database (clause_id 7) in
  equal_int_arrays [| 1; 2; -5; 6 |] (Array.map ~f:Literal.to_int clause)
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
  equal_int_arrays [| 0; 1; 3; 4; 7 |] (run 2)
  && equal_int_arrays [| 2; 6; 9 |] (run (-2))
  && equal_int_arrays [| 0; 4; 6 |] (run (-3))
  && equal_int_arrays [| 2; 4; 9 |] (run 5)
;;
