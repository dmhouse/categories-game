(** This module can be used to wrap an RPC protocol in order to provide a
   convenient way of testing both client and server in the same process. *)

open! Core_kernel
open Async_kernel
open Async_rpc_kernel

module type Rpc_repr = sig
  (** Users of this library must supply a representation type. Typically this
     will be a GADT that lists all of your RPCs. E.g.:

     {[
       type ('q, 'r) t =
         | Get_state : (Get_state.query, Get_state.response) t
         | Update_foo : (Update_foo.query, Update_foo.response) t
         | ...
     ]}

     If you have multiple versions of your RPCs (uncommon in web apps, since the
     client app is downloaded from the server each time), you could include a
     version number in this type as well. *)
  type ('q, 'r) t

  val rpc : ('q, 'r) t -> ('q, 'r) Rpc.Rpc.t
end

module type S = sig
  type ('q, 'r) repr
  type packed_repr = Pack : ('q, 'r) repr -> packed_repr

  module Call_rpc : sig
    (** A [Call_rpc.t] is a function that can be used by a bonsai app to call
       any RPC, like this:

       {[
          let get_state ~call_rpc =
            call_rpc.f Get_state ()
            |> Bonsai.Effect.inject ~on_response:(fun state -> ...)
       ]}
     *)
    type t = { f : 'q 'r. ('q, 'r) repr -> 'q -> 'r Or_error.t Bonsai.Effect.t }
  end

  (** If you have a connection to a server, then RPCs can be called by using
     dispatching them to the server. (Another way would be to feed them to an
     in-process copy of the server, useful during tests. See [For_testing]
     below.) *)
  val call_by_dispatching : Rpc.Connection.t -> Call_rpc.t

  module Handle_rpc : sig
    (** A [Handle_rpc.t] is a function that can respond to RPCs. *)

    type ('conn_state, 'q, 'r) handler =
      | Plain of ('conn_state -> 'q -> 'r)
      | Async of ('conn_state -> 'q -> 'r Deferred.t)

    type 'conn_state t = { f : 'q 'r. ('q, 'r) repr -> ('conn_state, 'q, 'r) handler }
  end

  (** If you give me a way to respond to RPCs, I can turn those into
     [Rpc_Implementation.t]'s for you. *)
  val implementations
    :  packed_repr list
    -> 'conn_state Handle_rpc.t
    -> 'conn_state Rpc.Implementation.t list

  module For_testing : sig
    (** A nice pattern for testing bonsai apps is:

        - Define a top level bonsai component [val bonsai : Call_rpc.t ->
       Result.t Bonsai.Component.t] (for some result type Result.t)

        - Your server library defines a [Handle_rpc.t].

        - In prod, one connects to the server passes [call_by_dispatching_exn
       conn] to [bonsai]. This can then be driven using Bonsai_web.Start in the
       normal way. Likewise, your server will use [val implementations] and
       create an actual RPC server.

        - To test this application, one can use [create_server], passing the
       [Handle_rpc] that your server library defined. The resulting [call_rpc.f]
       will feed the RPC directly to the [handle_rpc.f] rather than sending the
       RPC over the network. *)

    val create_server
      :  ?should_log:(packed_repr -> bool)
         (** By default, a message is written to stdout every time an RPC is
            called, with the RPC's name and version. You could use [should_log]
            to e.g. turn off logging for certain noisy RPCs. *)
      -> 'conn_state
      -> 'conn_state Handle_rpc.t
      -> Call_rpc.t
  end
end

module type Bonsai_rpc_wrapper = sig
  module type Rpc_repr = Rpc_repr
  module type S = S
  module Make (Rpc_repr : Rpc_repr) : S with type ('q, 'r) repr := ('q, 'r) Rpc_repr.t
end
