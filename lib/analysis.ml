open! Base

type t =
  { database : Database.t
  ; trail : Trail.t
  ; conflict_variable : Variable.t
  ; conflict_antecendent_dirty : Literal.t array
      (* dirty: may contain duplicates and includes the conflict pair *)
  ; conflict_step : int
  ; decision_step : int
  ; num_steps : int (* between last decision and conflict *)
  ; flow : int array
  ; mutable premises :
      (int, Int.comparator_witness) Set.t (* learned clause except uip variable *)
  ; mutable uip_literal : Literal.t option
  }

let get_conflict_variable (t : t) = t.conflict_variable
let get_num_steps (t : t) = t.num_steps
let get_uip_literal (t : t) = t.uip_literal

let get_learned_clause_exn (t : t) =
  match t.uip_literal with
  | Some x ->
    let int_uip_negated = Literal.to_int (Literal.negate x) in
    Literal.array_of_int_set (Set.add t.premises int_uip_negated)
  | None -> failwith "UIP not yet determined"
;;

let print (t : t) =
  Stdio.printf
    "conflict_variable = %s\nnum_steps = %d\npremises = [%s]\nuip_literal = %s\n"
    (Variable.show t.conflict_variable)
    t.num_steps
    (Util.show_list (Set.to_list t.premises) ~f:Int.to_string)
    (Literal.show (Option.value_exn t.uip_literal))
;;

let add_to_premises (t : t) (x : Literal.t) =
  t.premises <- Set.add t.premises (Literal.to_int x)
;;

let trail_ends_at_conflict (t : t) : bool =
  let var = Trail.last_step_variable_exn t.trail in
  let f x = Variable.equal (Variable.of_literal x) var in
  Array.exists t.conflict_antecendent_dirty ~f
  && not (Variable.equal var t.conflict_variable)
;;

let index (t : t) (var : Variable.t) : int option =
  let step = Trail.get_step t.trail var in
  let i = t.conflict_step - step in
  assert (i >= 0);
  if i < t.num_steps then Some i else None
;;

let partition (t : t) implied_var (xs : Literal.t array) : int list =
  let (parents : int list ref) = ref [] in
  let f x =
    let var = Variable.of_literal x in
    if not (Variable.equal var implied_var)
    then (
      match index t var with
      | Some i -> parents := i :: !parents
      | None -> add_to_premises t x)
  in
  Array.iter ~f xs;
  !parents
;;

let sum_flow_for_assert (flow : int array) (indices : int list) =
  (* because we did not remove duplicates from [conflict_antecendent_dirty] *)
  let uniqs = List.dedup_and_sort ~compare:Int.compare indices in
  List.fold uniqs ~init:0 ~f:(fun a i -> a + flow.(i))
;;

let split_flow (flow : int array) (input : int) (indices : int list) =
  match indices with
  | [] -> ()
  | [ i ] ->
    assert (input > 0);
    flow.(i) <- flow.(i) + input
  | _ ->
    let n = List.length indices in
    let q, r = input /% n, input % n in
    assert (q > 0);
    let f i j = flow.(j) <- (flow.(j) + if i < r then q + 1 else q) in
    List.iteri indices ~f
;;

let split_flow_assert (flow : int array) (input : int) (indices : int list) =
  let before = ref (-1) in
  assert (
    before := sum_flow_for_assert flow indices;
    !before >= 0);
  split_flow flow input indices;
  assert (!before + input = sum_flow_for_assert flow indices)
;;

let create database trail conflict =
  let conflict_variable = Conflict.variable conflict in
  let a1 = Database.get_literals database (Conflict.antecedent1 conflict) in
  let a2 = Database.get_literals database (Conflict.antecedent2 conflict) in
  let conflict_antecendent_dirty = Array.append a1 a2 in
  let conflict_step = Trail.length trail in
  let decision_step = Trail.get_last_decision_step_exn trail in
  let num_steps = 1 + conflict_step - decision_step in
  let flow = Array.create ~len:num_steps 0 in
  { database
  ; trail
  ; conflict_variable
  ; conflict_antecendent_dirty
  ; conflict_step
  ; decision_step
  ; num_steps
  ; flow
  ; premises = Set.empty (module Int)
  ; uip_literal = None
  }
;;

(** to short-circuit when a UIP is found *)
exception Short of Variable.t

let visit (t : t) i var cid =
  let input = t.flow.(i) in
  if input = Int.max_value then raise (Short var);
  let xs = Database.get_literals t.database cid in
  let parents = partition t var xs in
  if input > 0 then split_flow_assert t.flow input parents
;;

let analyze_conflict database trail conflict =
  let t = create database trail conflict in
  assert (trail_ends_at_conflict t);
  let conflict_parents = partition t t.conflict_variable t.conflict_antecendent_dirty in
  split_flow_assert t.flow Int.max_value conflict_parents;
  let uip : Variable.t =
    try
      Trail.iteri_down_until_last_decision t.trail ~f:(visit t);
      Trail.get_variable_at_step t.trail t.decision_step
    with
    | Short first_uip -> first_uip
  in
  let uip_value =
    match Trail.eval_variable t.trail uip with
    | True -> true
    | False -> false
    | Undefined -> failwith "undefined UIP value"
  in
  t.uip_literal <- Some (Variable.to_literal uip uip_value);
  t
;;
