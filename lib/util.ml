open! Base

let words line =
  line
  |> String.split_on_chars ~on:[ ' '; '\t' ]
  |> List.filter ~f:(Fn.non String.is_empty)
;;

let starts_with str c = (not (String.is_empty str)) && Char.equal str.[0] c