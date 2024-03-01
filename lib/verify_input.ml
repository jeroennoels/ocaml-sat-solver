open! Base

let verify dimacs =
  let len = Dimacs.num_variables dimacs in
  let pos = Array.create ~len false in
  let neg = Array.create ~len false in
  let seen x =
    if x = 0 || Int.abs x > len then failwith ("literal out of range: " ^ Int.to_string x);
    let arr = if x > 0 then pos else neg in
    let i = Int.abs x - 1 in
    arr.(i) <- true
  in
  Array.iter (Dimacs.clauses dimacs) ~f:(fun clause -> Array.iter clause ~f:seen);
  let find_false arr = Option.map ~f:fst (Array.findi arr ~f:(fun _ b -> not b)) in
  let fail_missing x = failwith ("missing literal: " ^ Int.to_string x) in
  match find_false pos, find_false neg with
  | Some x, _ -> fail_missing x
  | _, Some x -> fail_missing (-x)
  | None, None -> ()
;;
