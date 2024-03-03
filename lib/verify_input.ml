open! Base

let message text literal = text ^ ": " ^ Int.to_string literal

let verify dimacs =
  let nbvar = Dimacs.num_variables dimacs in
  let literals = Bi_array.create ~half:nbvar false in
  let seen x =
    if x = 0 || Int.abs x > nbvar
    then invalid_arg (message "literal out of range" x)
    else Bi_array.update literals x true
  in
  let process clause = Array.iter clause ~f:seen in
  Array.iter (Dimacs.clauses dimacs) ~f:process;
  match Bi_array.findi literals ~f:(fun _ b -> not b) with
  | Some (x, _) -> failwith (message "missing literal" x)
  | None -> ()
;;
