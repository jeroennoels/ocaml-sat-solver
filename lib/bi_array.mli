open! Base

type 'a t

val create : half:int -> 'a -> 'a t
val to_assoc : 'a t -> (int * 'a) list
val update : 'a t -> int -> 'a -> unit
val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option

(* a test may assert this to be identity on valid indexes *)
val identity : 'a t -> int -> int
