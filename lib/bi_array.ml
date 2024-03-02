open! Base

type 'a t =
  { neg : 'a array
  ; pos : 'a array
  }

let create ~half_len init =
  let alloc = Array.create ~len:half_len in
  { neg = alloc init; pos = alloc init }
;;

let to_assoc b =
  let x = Array.mapi b.neg ~f:(fun i a -> -i - 1, a) |> Array.to_list
  and y = Array.mapi b.pos ~f:(fun i a -> i + 1, a) |> Array.to_list in
  List.append x y
;;

let update b i a =
  match i with
  | i when i > 0 -> b.pos.(i - 1) <- a
  | 0 -> failwith "zero is not a valid index"
  | i -> b.neg.(-i - 1) <- a
;;

let findi b ~f =
  match Array.findi b.pos ~f:(fun j a -> f (j + 1) a) with
  | Some (i, a) -> Some (i + 1, a)
  | None ->
    (match Array.findi b.neg ~f:(fun j a -> f (-j - 1) a) with
     | Some (i, a) -> Some (-i - 1, a)
     | None -> None)
;;
