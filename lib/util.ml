open! Base

let words text =
  let split = String.split_on_chars ~on:[ ' '; '\t'; '\n' ] text in
  List.filter ~f:(Fn.non String.is_empty) split
;;

let starts_with str c = (not (String.is_empty str)) && Char.equal str.[0] c
