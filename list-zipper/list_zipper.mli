open Base

(** A particular implementation of zippers over lists. There is initially no
   focused element. Moving forward from the final element results in no focus,
   as does moving back from the first element. Moving forward from no focus will
   focus the first element, and moving back from no focus will focus the final
   element.
 *)

type 'a t [@@deriving sexp_of, compare]

val create : 'a list -> 'a t
val focus : 'a t -> 'a option
val next : 'a t -> 'a t
val prev : 'a t -> 'a t
val rev : 'a t -> 'a t
