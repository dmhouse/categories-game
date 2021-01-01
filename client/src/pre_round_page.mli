open! Core_kernel
open Import

(** A page that is shown before each round starts. The game owner can tweak the
   game's parameters, like which letter will be used. *)

val bonsai
  :  Random.State.t
  -> player_kind:Player_kind.t Bonsai.Value.t
  -> players:Player_id.Set.t Bonsai.Value.t
  -> game_id:Game_id.t Bonsai.Value.t
  -> categories_used_so_far:Category_id.Set.t Bonsai.Value.t
  -> Vdom.Node.t Bonsai.Computation.t
