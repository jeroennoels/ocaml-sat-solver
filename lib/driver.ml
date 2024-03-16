open! Base

let initialize cnf =
  let database = Database.create cnf in
  let nbvar = Database.num_variables database in
  let trail = Trail.create ~nbvar in
  let pipeline = Pipeline.create () in
  database, trail, pipeline
;;
