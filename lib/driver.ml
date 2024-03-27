open! Base

let initialize cnf =
  let database = Database.create cnf in
  let nbvar = Database.num_variables database in
  let trail = Trail.create ~nbvar in
  let pipeline = Pipeline.create () in
  database, trail, pipeline
;;

let run database trail pipeline : Conflict.t option =
  let rec go () =
    match Trail.is_complete trail with
    | true -> None
    | false ->
      let var = Trail.random_unassigned_exn trail in
      let x = Trail.decide trail var (Random.bool ()) in
      (match Propagation.propagate database trail pipeline x with
       | None -> go ()
       | conflict -> conflict)
  in
  go ()
;;

let analyze database trail (result : Conflict.t option) =
  match result with
  | None -> None
  | Some conflict -> Some (Analysis.analyze_conflict database trail conflict)
;;

(** short-circuit wheb SAT is observed *)
exception Short_sat of Trail.t

let drive database trail pipeline =
  let analysis =
    match run database trail pipeline with
    | None -> raise (Short_sat trail)
    | Some conflict -> Analysis.analyze_conflict database trail conflict
  in
  let clause = Analysis.get_learned_clause_exn analysis in
  Database.add_learned_clause database clause;
  let backjump_step = Analysis.calculate_backjump_step analysis in
  let v = Trail.get_variable_at_step trail backjump_step in
  Stdio.printf "\nbackjump step = %d, variable = %s\n" backjump_step (Variable.show v);
  Trail.backjump trail ~length:backjump_step;
  analysis
;;
