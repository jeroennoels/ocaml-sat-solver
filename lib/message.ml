open! Base

let message_str text arg = text ^ " [" ^ arg ^ "]"
let message_int text arg = message_str text (Int.to_string arg)
