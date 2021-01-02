open Core
open Async
open Import

module Conn_state : sig
  type t

  val create : unit -> t
end

type t

val create : Random.State.t -> Time_source.t -> t
val implementations : t -> Conn_state.t Rpc.Implementations.t

module For_testing : sig
  val handle_rpc : t -> ('q, 'r) Rpc_protocol.Which.t -> Conn_state.t -> 'q -> 'r
end
