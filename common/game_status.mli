open Core_kernel
open Types

(** Indicates what stage the game is in right now. *)

type t =
  | Pre_round of
      { players : Player_id.Set.t
      ; categories_used_so_far : Category_id.Set.t
            (** Contains all categories used in previous rounds for this game *)
      }
  | In_play of
      { time_remaining : Time_ns.Stable.Span.V2.t
      ; round_params : Round_params.t
      }
  | Results_presentation of
      { scores : Round_results.t
      ; running_totals : int Player_id.Map.t
      ; presentation_stage : Results_presentation_stage.t option
      ; round_params : Round_params.t
      }
[@@deriving sexp, bin_io, compare, equal]
