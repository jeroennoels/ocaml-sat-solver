open! Base

type option_bool =
  | True
  | False
  | Undefined

let negate = function
  | True -> False
  | False -> True
  | Undefined -> Undefined
;;
