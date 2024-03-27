open! Base

type variable = Variable.t
type literal = Literal.t
type clause_id = Clause_id.t
type clause = literal array

type t =
  { nbvar : int
  ; num_clauses : int
  ; mutable num_learned : int
  ; clauses : clause array
  ; learned_clauses : clause array
  ; positive : clause_id array array
  ; negative : clause_id array array
  }

let num_variables (t : t) = t.nbvar

let get_literals (t : t) cid =
  let i = Clause_id.to_int cid in
  if i < Array.length t.clauses
  then t.clauses.(i)
  else t.learned_clauses.(i - t.num_clauses)
;;

let get_clause_ids (t : t) x =
  let arr = if Literal.is_positive x then t.positive else t.negative in
  let v = Variable.of_literal x in
  arr.(Variable.to_int v)
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
  let to_arrays = Array.map ~f:Array.of_list in
  to_arrays neg, to_arrays pos
;;

let create cnf =
  let clauses = typeful_clauses cnf in
  let num_clauses = Array.length clauses in
  let invalid = Array.init 0 ~f:Literal.of_int_unchecked in
  let capacity = num_clauses in
  let learned_clauses = Array.create ~len:capacity invalid in
  let negative, positive = index_all cnf clauses in
  let nbvar = Cnf.num_variables cnf in
  { nbvar; num_clauses; num_learned = 0; clauses; learned_clauses; negative; positive }
;;

(** inefficient *)
let copy_extend (a : 'a array) (elt : 'a) =
  let len = Array.length a in
  (* time to optimize *)
  assert (len < 100);
  Array.init (len + 1) ~f:(fun i -> if i = len then elt else a.(i))
;;

let push_learned (t : t) (cid : clause_id) (x : literal) =
  let a = if Literal.is_positive x then t.positive else t.negative in
  let v = Variable.to_int (Variable.of_literal x) in
  a.(v) <- copy_extend a.(v) cid
;;

let add_learned_clause (t : t) xs =
  let i = t.num_learned in
  t.learned_clauses.(i) <- xs;
  let cid = Clause_id.of_int (t.num_clauses + i) in
  let f x = push_learned t cid x in
  Array.iter xs ~f;
  t.num_learned <- i + 1
;;
