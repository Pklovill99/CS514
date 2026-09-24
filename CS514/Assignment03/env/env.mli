type 'a t

val empty : 'a t
val extend : string -> 'a -> 'a t -> 'a t
val lookup : string -> 'a t -> 'a option
val is_bound : string -> 'a t -> bool

(** Bindings from innermost to outermost, shadowed names omitted. *)
val to_list : 'a t -> (string * 'a) list
