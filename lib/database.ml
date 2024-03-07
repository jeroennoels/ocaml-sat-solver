open! Base

type variable = Variable.t
type literal = Literal.t
type clause_id = Clause_id.t
type clause = literal array
type t = { clauses : clause array }

let get_clause t i = t.clauses.(Clause_id.to_int i)
let relevant_clauses _ _ = Array.create ~len:1 (Clause_id.of_int 0)

let typeful_clauses cnf : clause array =
  let nbvar = Cnf.num_variables cnf in
  let typeful x = Literal.of_int_check nbvar x in
  Array.map ~f:(Array.map ~f:typeful) (Cnf.clauses cnf)
;;

let push (arr : clause_id list array) (v : variable) (cid : clause_id) =
  arr.(Variable.to_int v) <- cid :: arr.(Variable.to_int v)
;;

let empty : clause_id list = []

(** The list [neg.(i)] holds variables that occur negatively in clause [i].
    Likewise for [pos] and positive occurrences.
    Position [i = 0] is wasted to avoid the offset. *)

let array_iter_cid ~f (arr : clause array) =
  let f i elt = f (Clause_id.of_int i) elt [@@inline] in
  Array.iteri ~f arr
;;

let index cnf (clauses : clause array) =
  let len = Cnf.num_variables cnf + 1 in
  let neg = Array.create ~len empty in
  let pos = Array.create ~len empty in
  let index_literal cid x =
    let arr = if Literal.is_positive x then pos else neg in
    push arr (Variable.of_literal x) cid
  in
  let index_clause cid clause = Array.iter ~f:(index_literal cid) clause in
  array_iter_cid ~f:index_clause clauses;
  neg, pos
;;

let create cnf =
  let clauses = typeful_clauses cnf in
  let _, _ = index cnf clauses in
  { clauses }
;;
