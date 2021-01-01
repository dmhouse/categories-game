open! Core_kernel
open Import

(** The initial landing page, which asks users to enter their username and the
   game ID they wish to join. *)

val bonsai
  :  log_in:(Rpc_protocol.Log_in.query -> unit Bonsai.Effect.t) Bonsai.Value.t
  -> Vdom.Node.t Bonsai.Computation.t
