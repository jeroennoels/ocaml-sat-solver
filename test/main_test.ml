open! Base
open! Stdio
open Sat

let%test "words" =
  let eq = List.equal String.equal in
  eq (Util.words "p cnf   25 \t66 ") [ "p"; "cnf"; "25"; "66" ]
  && eq (Util.words "\t-51 9 -243 88  0\t") [ "-51"; "9"; "-243"; "88"; "0" ]
;;

let%test "starts_with" =
  let p str = Util.starts_with str 'a' in
  p "abc" && p "a" && (not (p "")) && not (p "xyz")
;;
