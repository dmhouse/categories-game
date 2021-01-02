open Core_kernel
open Import

val go : unit -> unit

module For_testing : sig
  type call_rpc = { f : 'q 'r. ('q, 'r) Rpc_protocol.Which.t -> 'q -> 'r Bonsai.Effect.t }

  val bonsai
    :  Random.State.t
    -> Clock.t
    -> call_rpc:call_rpc
    -> on_error:(Error.t -> unit)
    -> Vdom.Node.t Bonsai.Computation.t
end
