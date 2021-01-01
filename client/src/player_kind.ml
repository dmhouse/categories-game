open! Core_kernel
open Import

type t =
  | Non_owner
  | Owner of { control_game : Rpc_protocol.Control_game.action -> unit Bonsai.Effect.t }
