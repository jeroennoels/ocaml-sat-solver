open! Base
open! Stdio
open Sat

let sample =
  let construct = Bi_array.create ~half_len:3 0 in
  let f i = Bi_array.update construct i (i * 10) in
  List.iter [ -3; -2; -1; 1; 2; 3 ] ~f;
  construct
;;

let%test "to_assoc" =
  List.equal
    (fun (i, x) (j, y) -> i = j && x = y)
    [ -1, -10; -2, -20; -3, -30; 1, 10; 2, 20; 3, 30 ]
    (Bi_array.to_assoc sample)
;;

let%test "find" =
  match Bi_array.findi sample ~f:(fun i a -> i * a = 40) with
  | Some (2, 20) -> true
  | Some (-2, -20) -> true
  | _ -> false
;;

let%test "not_found" = Option.is_none (Bi_array.findi sample ~f:Int.equal)
