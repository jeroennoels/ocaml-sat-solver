open! Base

type t = unit

let iter_reasons implied_var (xs : Literal.t array) ~(f : Variable.t -> unit) =
  let f x =
    let var = Variable.of_literal x in
    if Variable.equal var implied_var then () else f var
  in
  Array.iter ~f xs
;;

let analyze_conflict database trail conflict =
  let len = 1 + Trail.distance_to_last_decision trail in
  let flow = Array.create ~len 0 in
  let xs = Database.get_literals database (Conflict.antecedent1 conflict) in
  let ys = Database.get_literals database (Conflict.antecedent2 conflict) in
  let var = Conflict.variable conflict in
  iter_reasons var (Array.append xs ys) ~f:(fun _ -> ());
  ignore flow
;;
