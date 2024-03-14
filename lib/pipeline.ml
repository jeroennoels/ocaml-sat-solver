open! Base

type antecedent = Literal.t * Clause_id.t

(* we shall probably extend this towards efficient variable lookup *)
type t = { queue : antecedent Queue.t }

let empty () =
  let queue = Queue.create () in
  { queue }
;;

let dequeue (t : t) = Queue.dequeue t.queue

type detect =
  | New
  | Duplicate
  | Conflict of (antecedent * antecedent)

let same_literal (x, _) (y, _) = Literal.equal x y
let same_variable (x, _) (y, _) = Literal.same_variable x y

let find (t : t) (a : antecedent) : detect =
  match Queue.find t.queue ~f:(same_variable a) with
  | None -> New
  | Some b -> if same_literal a b then Duplicate else Conflict (a, b)
;;

let enqueue_all (t : t) units =
  let f a =
    match find t a with
    | New -> Queue.enqueue t.queue a
    | Duplicate -> ()
    | Conflict _ -> failwith "conflict"
  in
  List.iter ~f units;
  None
;;
