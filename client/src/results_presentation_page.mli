open! Core_kernel
open Import

(** The page where all players' submitted words are presented and scored. *)

val bonsai
  :  scores:Round_results.t Bonsai.Value.t
  -> presentation_stage:Results_presentation_stage.t option Bonsai.Value.t
  -> player_kind:Player_kind.t Bonsai.Value.t
  -> player_id:Player_id.t Bonsai.Value.t
  -> running_totals:int Player_id.Map.t Bonsai.Value.t
  -> Vdom.Node.t Bonsai.Computation.t
