type 'a t

exception Not_bound

val empty : 'a t
val extend : Syntax.id -> 'a -> 'a t -> 'a t
val extendback : Syntax.id -> 'a -> 'a t -> 'a t
val lookup : Syntax.id -> 'a t -> 'a
val map : ('a -> 'b) -> 'a t -> 'b t
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val print_env : ('a -> 'b) -> 'a t -> unit
