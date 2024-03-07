open! Base
open! Stdio
open Sat

let sample_cnf =
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
  cnf
;;

let database = Database.create sample_cnf
let clause_id = Clause_id.of_int
let equal_int_arrays = Array.equal Int.equal

let is_unboxed_int x =
  let r = Stdlib.Obj.repr x in
  Stdlib.Obj.is_int r && not (Stdlib.Obj.is_block r)
;;

let%test "unboxed" =
  let clause = Database.get_clause database (clause_id 0) in
  let x = clause.(0) in
  Literal.to_int x = 2 && is_unboxed_int x
;;

let%test "get clause" =
  let clause = Database.get_clause database (clause_id 5) in
  equal_int_arrays [| 1; 2; -3; -4; 5 |] (Array.map ~f:Literal.to_int clause)
;;
