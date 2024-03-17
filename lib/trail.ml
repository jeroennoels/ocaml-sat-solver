open! Base

type option_bool =
  | Undefined
  | False
  | True

let negate = function
  | True -> False
  | False -> True
  | Undefined -> Undefined
;;

type t =
  { steps_to_vars : int array
  ; vars_to_steps : int array
  ; mutable length : int
  ; mutable decision_level : int
  ; mutable log : bool
  }

let create ~nbvar =
  let len = nbvar + 1 in
  let steps_to_vars = Array.create ~len 0 in
  let vars_to_steps = Array.create ~len 0 in
  for i = 1 to nbvar do
    steps_to_vars.(i) <- i;
    vars_to_steps.(i) <- i
  done;
  { steps_to_vars; vars_to_steps; length = 0; decision_level = 0; log = false }
;;

let num_variables (t : t) = Array.length t.vars_to_steps - 1
let is_complete (t : t) = t.length = num_variables t

let invariant (t : t) =
  let f v s = t.steps_to_vars.(Int.abs s) = v in
  Array.for_alli ~f t.vars_to_steps
;;

let step_internal (t : t) x =
  assert (t.length < num_variables t);
  let var = Variable.of_literal x in
  let pos = Literal.is_positive x in
  let i = t.length + 1 in
  t.length <- i;
  let v = Variable.to_int var in
  let j = Int.abs t.vars_to_steps.(v) in
  if j < i then invalid_arg "variable is already assigned";
  let w = t.steps_to_vars.(i) in
  (* watch out for the special case i = j and v = w *)
  t.steps_to_vars.(i) <- v;
  t.steps_to_vars.(j) <- w;
  t.vars_to_steps.(v) <- (if pos then i else -i);
  t.vars_to_steps.(w) <- (if pos then j else -j)
;;

let step t (x, _) =
  if t.log then Stdio.printf "%d " (Literal.to_int x);
  step_internal t x
;;

let decide (t : t) v b =
  let x = Variable.to_literal v b in
  if t.log then Stdio.printf "[%d] " (Literal.to_int x);
  step_internal t x;
  t.decision_level <- t.decision_level + 1;
  x
;;

let backjump (t : t) ~length = t.length <- length

let eval_variable (t : t) var =
  let v = Variable.to_int var in
  let i = t.vars_to_steps.(v) in
  if i > 0
  then if i > t.length then Undefined else True
  else if -i > t.length
  then Undefined
  else False
;;

let eval_literal (t : t) x =
  let value = eval_variable t (Variable.of_literal x) in
  if Literal.is_positive x then value else negate value
;;

let is_assigned (t : t) x =
  match eval_literal t x with
  | Undefined -> false
  | _ -> true
;;

let random_unassigned_exn (t : t) =
  let nbvar = num_variables t in
  (* raises when bounds cross *)
  let i = Random.int_incl (t.length + 1) nbvar in
  Variable.of_int_check ~nbvar t.steps_to_vars.(i)
;;

let decision_level_exn _ _ = 0

let copy_unassigned (t : t) =
  let nbvar = num_variables t in
  let pos = t.length + 1 in
  let copy = Array.subo t.steps_to_vars ~pos in
  Array.sort ~compare:Int.compare copy;
  Array.map ~f:(Variable.of_int_check ~nbvar) copy
;;

let show_entry (i : int) (a : option_bool) =
  match a with
  | True -> "(" ^ Int.to_string i ^ ":T)"
  | False -> "(" ^ Int.to_string i ^ ":F)"
  | _ -> ""
;;

let show_assignment (t : t) =
  let nbvar = num_variables t in
  let variable i = Variable.of_int_check ~nbvar i in
  let f i = show_entry i (eval_variable t (variable i)) in
  let all_variables = List.range 1 ~stop:`inclusive nbvar in
  all_variables |> List.map ~f |> String.concat ~sep:""
;;

let length (t : t) = t.length
let set_logging (t : t) b = t.log <- b

let eval_literal_nodeps (t : t) i =
  let x = Literal.of_int_unchecked i in
  match eval_literal t x with
  | True -> Some true
  | False -> Some false
  | Undefined -> None
;;
