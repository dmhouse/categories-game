open Core
open Async

module Conn_state : sig
  type t

  val create : unit -> t
end

type t

val create : Random.State.t -> Time_source.t -> t
val implementations : t -> Conn_state.t Rpc.Implementations.t
