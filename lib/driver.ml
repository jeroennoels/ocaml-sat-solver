open! Base

let initialize cnf =
  let database = Database.create cnf in
  let nbvar = Database.num_variables database in
  let trail = Trail.create ~nbvar in
  let pipeline = Pipeline.create () in
  database, trail, pipeline
;;

let run database trail pipeline =
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

let analyze database trail result =
  match result with
  | None -> Stdio.print_endline "SAT"
  | Some conflict ->
    let todo = Analysis.analyze_conflict database trail conflict in
    ignore todo;
    Stdio.print_endline (Conflict.show conflict)
;;
