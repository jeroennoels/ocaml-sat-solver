open! Base

let initialize cnf =
  let database = Database.create cnf in
  let nbvar = Database.num_variables database in
  let trail = Trail.create ~nbvar in
  let pipeline = Pipeline.create () in
  database, trail, pipeline
;;

let run database trail pipeline (kickoff : (Literal.t * Clause_id.t) option)
  : Conflict.t option
  =
  let rec go ko =
    match Trail.is_complete trail with
    | true -> None
    | false ->
      let x =
        match ko with
        | None ->
          let var = Trail.random_unassigned_exn trail in
          Trail.decide trail var (Random.bool ())
        | Some ((y, _) as learned) ->
          Trail.step trail learned;
          y
      in
      (match Propagation.propagate database trail pipeline x with
       | None -> go None
       | conflict -> conflict)
  in
  go kickoff
;;

let analyze database trail (result : Conflict.t option) =
  match result with
  | None -> None
  | Some conflict -> Some (Analysis.analyze_conflict database trail conflict)
;;

(** short-circuit wheb SAT is observed *)
exception Short_sat of Trail.t

let drive database trail pipeline kickoff =
  let analysis =
    match run database trail pipeline kickoff with
    | None -> raise (Short_sat trail)
    | Some conflict -> Analysis.analyze_conflict database trail conflict
  in
  let clause = Analysis.get_learned_clause_exn analysis in
  let learned_clause_id = Database.add_learned_clause database clause in
  let backjump_step = Analysis.calculate_backjump_step analysis in
  (* let v = Trail.get_variable_at_step trail backjump_step in *)
  (* Stdio.printf "\nbackjump step = %d, variable = %s\n" backjump_step (Variable.show v); *)
  Trail.backjump trail ~length:backjump_step;
  analysis, learned_clause_id
;;

let solve database trail pipeline =
  let rec loop kickoff =
    let analysis, learned_clause_id = drive database trail pipeline kickoff in
    loop
    @@
    match Analysis.get_uip_literal analysis with
    | Some x -> Some (Literal.negate x, learned_clause_id)
    | None -> None
  in
  loop None
;;
