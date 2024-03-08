open! Base

let words line =
  line
  |> String.split_on_chars ~on:[ ' '; '\t' ]
  |> List.filter ~f:(Fn.non String.is_empty)
;;

let starts_with str c = (not (String.is_empty str)) && Char.equal str.[0] c
let show_array a ~f = String.concat ~sep:" " (List.map ~f (Array.to_list a))
let show_pair (x, y) ~f ~g = "(" ^ f x ^ ", " ^ g y ^ ")"
