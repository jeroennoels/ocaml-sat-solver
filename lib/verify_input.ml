open! Base

exception Invalid_cnf of string

let invalid_cnf msg = raise (Invalid_cnf msg)
let message text i = text ^ ": " ^ Int.to_string i
let clause_repeats variable = invalid_cnf (message "clause repeats variable" variable)

let check_2 clause =
  if Int.abs clause.(0) = Int.abs clause.(1) then clause_repeats clause.(0) else ()
;;

let check_3 clause =
  let x = Int.abs clause.(0)
  and y = Int.abs clause.(1)
  and z = Int.abs clause.(2) in
  if x = y || x = z then clause_repeats x else if y = z then clause_repeats z else ()
;;

let check_n clause =
  let abs_copy = Array.map ~f:Int.abs clause in
  Array.sort abs_copy ~compare:Int.compare;
  match Array.find_consecutive_duplicate abs_copy ~equal:Int.equal with
  | Some (x, _) -> clause_repeats x
  | None -> ()
;;

let check clause =
  match Array.length clause with
  | 0 -> invalid_cnf "empty clause"
  | 1 -> invalid_cnf (message "trivial clause" clause.(0))
  | 2 -> check_2 clause
  | 3 -> check_3 clause
  | _ -> check_n clause
;;

let verify dimacs =
  let nbvar = Dimacs.num_variables dimacs in
  let literals = Bi_array.create ~half:nbvar false in
  let seen x =
    if x = 0 || Int.abs x > nbvar
    then invalid_cnf (message "literal out of range" x)
    else Bi_array.update literals x true
  in
  let process clause =
    check clause;
    Array.iter clause ~f:seen
  in
  Array.iter (Dimacs.clauses dimacs) ~f:process;
  match Bi_array.findi literals ~f:(fun _ b -> not b) with
  | Some (x, _) -> invalid_cnf (message "missing literal" x)
  | None -> ()
;;
