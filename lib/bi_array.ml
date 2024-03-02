open! Base

type 'a t = 'a array

let create ~half a = Array.create ~len:(2 * half) a

let encode t = function
  | 0 -> invalid_arg "zero is not a valid index"
  | i -> (Array.length t / 2) + if i > 0 then i - 1 else i
;;

let decode t i =
  let half = Array.length t / 2 in
  if i < half then i - half else i - half + 1
;;

let to_assoc t =
  let f i a = decode t i, a in
  Array.to_list (Array.mapi t ~f)
;;

let update t i a = t.(encode t i) <- a

let findi t ~f =
  let f i a = f (decode t i) a in
  match Array.findi t ~f with
  | Some (i, a) -> Some (decode t i, a)
  | None -> None
;;

let identity t i = decode t (encode t i)
