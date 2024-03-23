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

(* all our arrays below waste index zero to avoid offsets *)

type t =
  { step_to_var : int array
  ; var_to_step : int array
  ; step_to_clause_id : int array (* only a part of this array will be relevant *)
  ; mutable length : int
  ; mutable last_decision_step : int option (* option until we handle backjumps *)
  ; mutable log : bool
  }

let create ~nbvar =
  let len = nbvar + 1 in
  let step_to_var = Array.create ~len 0 in
  let var_to_step = Array.create ~len 0 in
  let step_to_clause_id = Array.create ~len Clause_id.invalid_as_int in
  for i = 1 to nbvar do
    step_to_var.(i) <- i;
    var_to_step.(i) <- i
  done;
  { step_to_var
  ; var_to_step
  ; step_to_clause_id
  ; length = 0
  ; last_decision_step = None
  ; log = false
  }
;;

let num_variables (t : t) = Array.length t.var_to_step - 1
let is_complete (t : t) = t.length = num_variables t

let invariant (t : t) =
  let f v s = t.step_to_var.(Int.abs s) = v in
  Array.for_alli ~f t.var_to_step
;;

let step_internal (t : t) (x : Literal.t) : unit =
  assert (t.length < num_variables t);
  let var = Variable.of_literal x in
  let pos = Literal.is_positive x in
  let i = t.length + 1 in
  t.length <- i;
  let v = Variable.to_int var in
  let j = Int.abs t.var_to_step.(v) in
  if j < i then invalid_arg "variable is already assigned";
  let w = t.step_to_var.(i) in
  (* watch out for the special case i = j and v = w *)
  t.step_to_var.(i) <- v;
  t.step_to_var.(j) <- w;
  t.var_to_step.(v) <- (if pos then i else -i);
  t.var_to_step.(w) <- (if pos then j else -j)
;;

let step (t : t) (x, cid) : unit =
  if t.log then Stdio.printf "%d " (Literal.to_int x);
  step_internal t x;
  t.step_to_clause_id.(t.length) <- Clause_id.to_int cid
;;

let decide (t : t) v b =
  let x = Variable.to_literal v b in
  if t.log then Stdio.printf "[%d] " (Literal.to_int x);
  step_internal t x;
  t.last_decision_step <- Some t.length;
  t.step_to_clause_id.(t.length) <- Clause_id.invalid_as_int;
  x
;;

let get_step (t : t) var = Int.abs t.var_to_step.(Variable.to_int var)

let backjump (t : t) ~length =
  t.length <- length;
  t.last_decision_step <- None
;;

let eval_variable (t : t) var =
  let v = Variable.to_int var in
  let i = t.var_to_step.(v) in
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
  Variable.of_int_check ~nbvar t.step_to_var.(i)
;;

let get_last_decision_step_exn (t : t) : int =
  match t.last_decision_step with
  | None -> failwith "last decision is undefined"
  | Some decision_step -> decision_step
;;

let iter_down_until_last_decision (t : t) ~f =
  let decision_step = get_last_decision_step_exn t in
  assert (t.length >= decision_step);
  let rec go i =
    if i > decision_step
    then (
      let var = Variable.of_int_unchecked t.step_to_var.(i) in
      let cid = Clause_id.of_int t.step_to_clause_id.(i) in
      f var cid;
      go (i - 1))
  in
  go t.length
;;

let last_step_variable_exn (t : t) =
  if t.length > 0
  then Variable.of_int_unchecked t.step_to_var.(t.length)
  else invalid_arg "empty trail"
;;

let copy_unassigned (t : t) =
  let nbvar = num_variables t in
  let pos = t.length + 1 in
  let copy = Array.subo t.step_to_var ~pos in
  Array.sort ~compare:Int.compare copy;
  Array.map ~f:(Variable.of_int_check ~nbvar) copy
;;

let copy_assigned (t : t) =
  let nbvar = num_variables t in
  (* remember: index zero is not used *)
  let copy = Array.sub t.step_to_var ~pos:1 ~len:t.length in
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
