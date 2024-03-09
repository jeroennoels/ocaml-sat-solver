open! Base

type option_bool =
  | Undefined
  | False
  | True

type t = option_bool array

let negate = function
  | True -> False
  | False -> True
  | Undefined -> Undefined
;;

let create ~nbvar : t = Array.create ~len:(nbvar + 1) Undefined

let eval (t : t) x =
  let i = Literal.to_int x in
  if i > 0 then t.(i) else negate t.(-i)
;;

let is_undefined = function
  | Undefined -> true
  | _ -> false
;;

let assign (t : t) v b =
  assert (is_undefined t.(Variable.to_int v));
  t.(Variable.to_int v) <- (if b then True else False)
;;

let forget (t : t) v = t.(Variable.to_int v) <- Undefined

let show_entry i a =
  match a with
  | True -> "(" ^ Int.to_string i ^ ":T)"
  | False -> "(" ^ Int.to_string i ^ ":F)"
  | _ -> ""
;;

let show (t : t) = Array.mapi ~f:show_entry t |> Array.to_list |> String.concat ~sep:""
