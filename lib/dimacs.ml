open! Base

type t =
  { num_variables : int
  ; clauses : int list array
  ; mutable count : int
  }

let num_variables t = t.num_variables
let num_clauses t = Array.length t.clauses
let clauses t = t.clauses
let is_problem line = Util.starts_with line 'p'
let is_comment line = Util.starts_with line 'c'

let parse_problem line : (t, string) Result.t =
  match Util.words line with
  | [ "p"; "cnf"; v; c ] ->
    let len = Int.of_string c in
    Ok { num_variables = Int.of_string v; clauses = Array.create ~len []; count = 0 }
  | _ -> Error "invalid problem header"
;;

let parse_clause line = List.map ~f:Int.of_string (Util.words line) |> List.drop_last_exn

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
    t.clauses.(t.count) <- parse_clause line;
    t.count <- t.count + 1;
    build
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
