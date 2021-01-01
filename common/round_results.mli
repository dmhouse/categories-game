open Core_kernel
open Types

(** Represents the scores resulting from one round of the game. *)

type submission =
  { word : Word.t option
  ; status : Word_status.t
  ; score : int
  }
[@@deriving sexp, compare, equal, bin_io]

type t =
  { total_scores : int Player_id.Map.t
  ; by_category : submission Player_id.Map.t Category_id.Map.t
  }
[@@deriving sexp, compare, equal, bin_io]

val compute
  :  player_responses:Word.t option Category_id.Map.t Player_id.Map.t
  -> categories:Category_id.t list
  -> disallowed_words:Player_and_category.Set.t
       (** The server keeps track of words have been disallowed by the game owner.
     These are not counted as a submission. *)
  -> t
