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

type p_line =
  { nbvar : int
  ; nbclauses : int
  }

let parse_problem line : (p_line, string) Result.t =
  match Util.words line with
  | [ "p"; "cnf"; v; c ] -> Ok { nbvar = Int.of_string v; nbclauses = Int.of_string c }
  | _ -> Error "invalid problem header"
;;

let initialize { nbvar; nbclauses } : t =
  let empty = Array.create ~len:0 0 in
  let clauses = Array.create ~len:nbclauses empty in
  { num_variables = nbvar; clauses; count = 0 }
;;

(** Assume literals are separated by single spaces. *)
let parse_clause line : int array option =
  let tokens = String.split line ~on:' ' in
  let len = List.length tokens - 1 in
  let terminated = ref false in
  let clause = Array.create ~len 0 in
  List.iteri tokens ~f:(fun i token ->
    if i < len
    then clause.(i) <- Int.of_string token
    else terminated := String.equal token "0");
  if !terminated && len > 0 then Some clause else None
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
     | Ok p_line -> Build (initialize p_line)
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
  let parse = Stdio.In_channel.fold_lines ~init:Intro ~f:accumulate in
  Stdio.In_channel.with_file filename ~f:parse |> to_result
;;
