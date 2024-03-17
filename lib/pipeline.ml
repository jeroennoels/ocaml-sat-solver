open! Base

type antecedent = Literal.t * Clause_id.t

(* We shall probably extend this record type with an additional
   data structure to make variable lookup more efficient. *)

type t =
  { queue : antecedent Queue.t
  ; mutable log : bool
  }

let create () =
  let queue = Queue.create () in
  { queue; log = false }
;;

let bar n = String.init n ~f:(Fn.const '#')
let set_logging (t : t) b = t.log <- b
let length (t : t) = Queue.length t.queue
let print_bar (t : t) = Stdio.print_endline (bar (length t))
let clear (t : t) = Queue.clear t.queue
let is_empty (t : t) = Queue.is_empty t.queue

let dequeue (t : t) =
  if t.log then print_bar t;
  Queue.dequeue t.queue
;;

type detect =
  | New
  | Duplicate
  | Conflict of Conflict.t

let same_literal (x, _) (y, _) = Literal.equal x y [@@inline]
let same_variable (x, _) (y, _) = Literal.same_variable x y [@@inline]

(* the following approach will be inefficient when the queue is large *)

let find (t : t) (a : antecedent) : detect =
  (* time to optimize *)
  assert (length t < 100);
  match Queue.find t.queue ~f:(same_variable a) with
  | None -> New
  | Some b ->
    if same_literal a b
    then Duplicate
    else (
      let _, kappa = a in
      Conflict (Conflict.create b kappa))
;;

(** to short-circuit when a conflict is detected *)
exception Short of Conflict.t

let enqueue_all (t : t) units =
  if t.log then print_bar t;
  let f a =
    match find t a with
    | New -> Queue.enqueue t.queue a
    | Duplicate -> ()
    | Conflict c -> raise (Short c)
  in
  try
    List.iter ~f units;
    None
  with
  | Short conflict -> Some conflict
;;
