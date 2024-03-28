open! Base

type variable = Variable.t
type literal = Literal.t
type clause_id = Clause_id.t
type clause = literal array
type clauses = int Hash_set.t

type t =
  { nbvar : int
  ; num_clauses : int
  ; mutable num_learned : int
  ; clauses : clause array
  ; learned_clauses : clause array
  ; positive : clauses array
  ; negative : clauses array
  }

let num_variables (t : t) = t.nbvar

let get_literals (t : t) cid =
  let i = Clause_id.to_int cid in
  if i < Array.length t.clauses
  then t.clauses.(i)
  else t.learned_clauses.(i - t.num_clauses)
;;

(* for testing only *)
let get_clause_ids (t : t) x =
  let arr = if Literal.is_positive x then t.positive else t.negative in
  let v = Variable.of_literal x in
  Array.map ~f:Clause_id.of_int (Hash_set.to_array arr.(Variable.to_int v))
;;

let fold_over_clauses_containing (t : t) x ~f =
  let arr = if Literal.is_positive x then t.positive else t.negative in
  let v = Variable.of_literal x in
  let g acc i =
    let cid = Clause_id.of_int i in
    f acc (get_literals t cid) cid
  in
  Hash_set.fold ~init:[] ~f:g arr.(Variable.to_int v)
;;

let typeful_clauses cnf : clause array =
  let nbvar = Cnf.num_variables cnf in
  let typeful x = Literal.of_int_check ~nbvar x in
  Array.map ~f:(Array.map ~f:typeful) (Cnf.clauses cnf)
;;

let push (arr : clause_id list array) (v : variable) (cid : clause_id) =
  arr.(Variable.to_int v) <- cid :: arr.(Variable.to_int v)
;;

(** The list [neg.(i)] holds variables that occur negatively in clause [i].
    Likewise for [pos] and positive occurrences.
    Position [i = 0] is wasted to avoid the offset. *)

let array_iter_cid ~f (arr : clause array) =
  let f i elt = f (Clause_id.of_int i) elt [@@inline] in
  Array.iteri ~f arr
;;

let empty : clause_id list = []

let index_all cnf (clauses : clause array) =
  let len = Cnf.num_variables cnf + 1 in
  let neg = Array.create ~len empty in
  let pos = Array.create ~len empty in
  let index_literal cid x =
    let arr = if Literal.is_positive x then pos else neg in
    push arr (Variable.of_literal x) cid
  in
  let index_clause cid clause = Array.iter ~f:(index_literal cid) clause in
  array_iter_cid ~f:index_clause clauses;
  let f cids = Hash_set.of_list (module Int) (List.map ~f:Clause_id.to_int cids) in
  let to_sets = Array.map ~f in
  to_sets neg, to_sets pos
;;

let create cnf =
  let clauses = typeful_clauses cnf in
  let num_clauses = Array.length clauses in
  let invalid = Array.init 0 ~f:Literal.of_int_unchecked in
  let capacity = 2 * num_clauses in
  let learned_clauses = Array.create ~len:capacity invalid in
  let negative, positive = index_all cnf clauses in
  let nbvar = Cnf.num_variables cnf in
  { nbvar; num_clauses; num_learned = 0; clauses; learned_clauses; negative; positive }
;;

let push_learned (t : t) (cid : clause_id) (x : literal) =
  let a = if Literal.is_positive x then t.positive else t.negative in
  let v = Variable.to_int (Variable.of_literal x) in
  Hash_set.add a.(v) (Clause_id.to_int cid)
;;

let add_learned_clause (t : t) xs : clause_id =
  let i = t.num_learned in
  t.learned_clauses.(i) <- xs;
  let cid = Clause_id.of_int (t.num_clauses + i) in
  let f x = push_learned t cid x in
  Array.iter xs ~f;
  t.num_learned <- i + 1;
  cid
;;
