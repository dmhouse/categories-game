open Core_kernel
module Category_id : Identifiable

(** A "word" is a response submitted by a player. (Of course, this might
   actually be more than one English language word.) *)
module Word : Identifiable

module Player_id : Identifiable

(** Each game is given a unique ID upon creation by the game owner *)
module Game_id : Identifiable

module Round_params : sig
  (** Indicates the parameters for a single round of the game *)
  type t =
    { letter : char
    ; length : Time_ns.Span.t
    ; categories : Category_id.Set.t
    }
  [@@deriving sexp, bin_io, compare, equal]
end

module Word_status : sig
  (** Words can be disallowed by the game owner if the players don't feel like
     it it a valid entry for the category. *)
  type t =
    | Allowed
    | Disallowed
  [@@deriving sexp, compare, equal, bin_io]
end

module Player_and_category : sig
  type t =
    { player : Player_id.t
    ; category : Category_id.t
    }

  include Comparable.S with type t := t
end

module Results_presentation_stage : sig
  (** The results are presented to users one category at a time, with a final
     screen at the end with the overall scores. *)

  type t =
    | Category of Category_id.t
    | Total_scores
  [@@deriving sexp, bin_io, compare]
end
