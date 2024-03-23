open! Base

val words : string -> string list
val starts_with : string -> char -> bool
val show_list : 'a list -> f:('a -> string) -> string
val show_array : 'a array -> f:('a -> string) -> string
val show_pair : 'a * 'b -> f:('a -> string) -> g:('b -> string) -> string
