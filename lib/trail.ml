open! Base

type t =
  { assignment : Assignment.t
  ; steps_to_vars : int array
  ; vars_to_steps : int array
  ; mutable length : int
  }

let empty ~nbvar =
  let len = nbvar + 1 in
  let assignment = Assignment.create ~nbvar in
  let steps_to_vars = Array.create ~len 0 in
  let vars_to_steps = Array.create ~len 0 in
  for i = 1 to nbvar do
    steps_to_vars.(i) <- i;
    vars_to_steps.(i) <- i
  done;
  { assignment; steps_to_vars; vars_to_steps; length = 0 }
;;

let assignment t = t.assignment
let decide _ = None

let step t (x, _) =
  let var = Variable.of_literal x in
  Assignment.assign t.assignment var (Literal.is_positive x);
  let i = t.length + 1 in
  t.length <- i;
  let v = Variable.to_int var in
  let j = t.vars_to_steps.(v) in
  let w = t.steps_to_vars.(i) in
  t.steps_to_vars.(i) <- v;
  t.steps_to_vars.(j) <- w;
  t.vars_to_steps.(v) <- i;
  t.vars_to_steps.(w) <- j
;;

let decision_level_exn _ _ = 0
