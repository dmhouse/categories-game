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
  | Owner of { control_game : Rpc_protocol.Control_game.action -> unit Bonsai.Effect.t }
