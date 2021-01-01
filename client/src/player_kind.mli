open! Core_kernel
open Import

(** The first player to create a game of categories is the "owner", who has more
   control than the rest of the players:

   - Before each starts, they are able to tweak the parameters like the letter
     that will be used
   - They control the results presentation and can page back and forth.
   - They are in charge of allowing or disallowing words.
 *)

type t =
  | Non_owner
  | Owner of
      { start_round : Round_params.t -> unit Bonsai.Effect.t
            (** Once the game details have been chosen on the pre-round page, this
                starts the round. *)
      ; advance_results_presentation : [ `Forward | `Back ] -> unit Bonsai.Effect.t
      ; next_round : unit Bonsai.Effect.t
            (** Once the results have been fully presented, this moves the game state
                back to the pre-round page. *)
      ; set_word_status :
          Word_status.t
          -> player:Player_id.t
          -> category:Category_id.t
          -> unit Bonsai.Effect.t
            (** Used during the results presentation to either allow or disallow
                a given submission. *)
      }
