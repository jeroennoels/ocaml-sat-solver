open! Base

type t =
  { num_variables : int
  ; clauses : int array array
  ; mutable count : int
  }

let num_variables t = t.num_variables
let num_clauses t = Array.length t.clauses
let clauses t = t.clauses
let is_problem line = Util.starts_with line 'p'
let is_comment line = Util.starts_with line 'c'
let empty = Array.create ~len:0 0

let parse_problem line : (t, string) Result.t =
  match Util.words line with
  | [ "p"; "cnf"; v; c ] ->
    let len = Int.of_string c in
    Ok { num_variables = Int.of_string v; clauses = Array.create ~len empty; count = 0 }
  | _ -> Error "invalid problem header"
;;

let parse_clause line =
  let ws = Util.words line in
  let len = List.length ws - 1 in
  if len < 1
  then None
  else (
    let clause = Array.create ~len 0 in
    List.iteri ws ~f:(fun i w -> if i < len then clause.(i) <- Int.of_string w);
    Some clause)
;;

type accumulator =
  | Intro
  | Build of t
  | Fail of string

let accumulate : accumulator -> string -> accumulator =
  fun acc line ->
  match acc with
  | Intro when is_comment line -> Intro
  | Intro when is_problem line ->
    (match parse_problem line with
     | Ok t -> Build t
     | Error msg -> Fail msg)
  | Intro -> Fail "missing problem line"
  | Build t when t.count >= num_clauses t -> Fail "too many clauses"
  | Build t as build ->
    (match parse_clause line with
     | Some clause ->
       t.clauses.(t.count) <- clause;
       t.count <- t.count + 1;
       build
     | None -> Fail ("invalid clause line: " ^ line))
  | Fail _ as fail -> fail
;;

let to_result : accumulator -> (t, string) Result.t = function
  | Build t when t.count = num_clauses t -> Ok t
  | Build t ->
    assert (t.count < num_clauses t);
    Error "not enough clauses"
  | Fail msg -> Result.fail msg
  | Intro -> Result.fail "missing problem header"
;;

let read_lines lines = List.fold lines ~init:Intro ~f:accumulate |> to_result

let read_file filename =
  let parse = Core.In_channel.fold_lines ~init:Intro ~f:accumulate in
  Core.In_channel.with_file filename ~f:parse |> to_result
;;
