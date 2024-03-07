open! Base

type variable = Variable.t
type literal = Literal.t
type clause_id = Clause_id.t
type clause = literal array

type t =
  { clauses : clause array
  ; positive : clause_id array array
  ; negative : clause_id array array
  }

let get_clause t i = t.clauses.((Clause_id.to_int [@Inlined]) i)

let relevant_clauses t x =
  let arr = if (Literal.is_positive [@Inlined]) x then t.negative else t.positive in
  let v = (Variable.of_literal [@Inlined]) x in
  arr.((Variable.to_int [@Inlined]) v)
;;

let typeful_clauses cnf : clause array =
  let nbvar = Cnf.num_variables cnf in
  let typeful x = Literal.of_int_check nbvar x in
  Array.map ~f:(Array.map ~f:typeful) (Cnf.clauses cnf)
;;

let push (arr : clause_id list array) (v : variable) (cid : clause_id) =
  arr.(Variable.to_int v) <- cid :: arr.(Variable.to_int v)
;;

(** The list [neg.(i)] holds variables that occur negatively in clause [i].
    Likewise for [pos] and positive occurrences.
    Position [i = 0] is wasted to avoid the offset. *)

let array_iter_cid ~f (arr : clause array) =
  let f i elt = f ((Clause_id.of_int [@Inlined]) i) elt [@@inline] in
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
  let negative, positive = index_all cnf clauses in
  { clauses; negative; positive }
;;
