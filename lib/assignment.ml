open! Base

type option_bool =
  | Undefined
  | False
  | True

type t = int array

let negate = function
  | True -> False
  | False -> True
  | Undefined -> Undefined
;;

let create ~nbvar = Array.create ~len:(nbvar + 1) 0

let eval t x =
  let i = Literal.to_int x in
  if i > 0
  then (
    match t.(i) with
    | 1 -> True
    | -1 -> False
    | _ -> Undefined)
  else (
    match t.(-i) with
    | -1 -> True
    | 1 -> False
    | _ -> Undefined)
;;

let assign t v b =
  assert (t.(Variable.to_int v) = 0);
  t.(Variable.to_int v) <- (if b then 1 else -1)
;;

let forget t v = t.(Variable.to_int v) <- 0

let show_entry i a =
  match a with
  | 1 -> "(" ^ Int.to_string i ^ ":T)"
  | -1 -> "(" ^ Int.to_string i ^ ":F)"
  | _ -> ""
;;

let show t = Array.mapi ~f:show_entry t |> Array.to_list |> String.concat ~sep:""
