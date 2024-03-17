open! Base

type t =
  { num_variables : int
  ; clauses : int array array
  }

let create num_variables clauses = { num_variables; clauses }
let num_variables t = t.num_variables
let num_clauses t = Array.length t.clauses
let clauses t = t.clauses

let satisfied (eval_literal : int -> bool option) clause =
  let is_true x =
    match eval_literal x with
    | Some true -> true
    | _ -> false
  in
  Array.exists ~f:is_true clause
;;

let conflicting (eval_literal : int -> bool option) clause =
  let is_false x =
    match eval_literal x with
    | Some false -> true
    | _ -> false
  in
  Array.for_all ~f:is_false clause
;;

type counters =
  { mutable s : int
  ; mutable c : int
  ; mutable u : int
  }

let num_satisfied { s; _ } = s
let num_conflicting { c; _ } = c
let num_undecided { u; _ } = u

let show_counters { s; c; u } =
  Printf.sprintf "satisfied: %d, conflicting: %d, undecided: %d" s c u
;;

let evaluate (t : t) (eval : int -> bool option) =
  let count = { s = 0; c = 0; u = 0 } in
  for i = 0 to num_clauses t - 1 do
    let clause = t.clauses.(i) in
    if satisfied eval clause
    then count.s <- count.s + 1
    else if conflicting eval clause
    then count.c <- count.c + 1
    else count.u <- count.u + 1
  done;
  count
;;
