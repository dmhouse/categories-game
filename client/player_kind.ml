open! Core_kernel
open Import

type t =
  | Non_owner
  | Owner of
      { start_round : Round_params.t -> unit Bonsai.Effect.t
      ; advance_results_presentation : [ `Forward | `Back ] -> unit Bonsai.Effect.t
      ; next_round : unit Bonsai.Effect.t
      ; set_word_status :
          Word_status.t
          -> player:Player_id.t
          -> category:Category_id.t
          -> unit Bonsai.Effect.t
      }
