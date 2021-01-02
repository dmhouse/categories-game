open Core_kernel
open Import

val go : unit -> unit

module For_testing : sig
  val bonsai
    :  Random.State.t
    -> Clock.t
    -> call_rpc:Rpc_protocol.Which.Call_rpc.t
    -> on_error:(Error.t -> unit)
    -> Vdom.Node.t Bonsai.Computation.t
end
