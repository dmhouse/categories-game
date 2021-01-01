open Core
open Import

type t [@@deriving sexp_of]

val create : Random.State.t -> owner:Player_id.t -> id:Game_id.t -> t
val owner : t -> Player_id.t
val add_player : t -> Player_id.t -> unit

val start_round
  :  t
  -> now:Time_ns.t
  -> round_params:Round_params.t
  -> Time_ns.t Or_error.t

val submit_words : t -> Player_id.t -> Word.t option Category_id.Map.t -> unit Or_error.t
val end_round : t -> unit
val advance_results_presentation : t -> [ `Forward | `Back ] -> unit Or_error.t

val set_word_status
  :  t
  -> player:Player_id.t
  -> category:Category_id.t
  -> status:Word_status.t
  -> unit Or_error.t

val next_round : t -> unit Or_error.t
val status : t -> Game_status.t
