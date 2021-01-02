open Core_kernel
open Async_kernel
open Async_rpc_kernel

module type Rpc_repr = sig
  type ('q, 'r) t

  val rpc : ('q, 'r) t -> ('q, 'r) Rpc.Rpc.t
end

module type S = sig
  type ('q, 'r) repr
  type packed_repr = Pack : ('q, 'r) repr -> packed_repr

  module Call_rpc : sig
    type t = { f : 'q 'r. ('q, 'r) repr -> 'q -> 'r Bonsai.Effect.t }
  end

  val call_by_dispatching : Rpc.Connection.t -> Call_rpc.t

  module Handle_rpc : sig
    type ('conn_state, 'q, 'r) handler =
      | Plain of ('conn_state -> 'q -> 'r)
      | Async of ('conn_state -> 'q -> 'r Deferred.t)

    type 'conn_state t = { f : 'q 'r. ('q, 'r) repr -> ('conn_state, 'q, 'r) handler }
  end

  val implementations
    :  packed_repr list
    -> 'conn_state Handle_rpc.t
    -> 'conn_state Rpc.Implementation.t list

  module For_testing : sig
    val create_server : 'conn_state -> 'conn_state Handle_rpc.t -> Call_rpc.t
  end
end

module Make (Rpc_repr : Rpc_repr) : S with type ('q, 'r) repr := ('q, 'r) Rpc_repr.t =
struct
  type packed_repr = Pack : ('q, 'r) Rpc_repr.t -> packed_repr

  module Call_rpc = struct
    type t = { f : 'q 'r. ('q, 'r) Rpc_repr.t -> 'q -> 'r Bonsai.Effect.t }
  end

  let call_by_dispatching conn =
    { Call_rpc.f =
        (fun (type q r) (repr : (q, r) Rpc_repr.t) (query : q) ->
          let f =
            Bonsai_web.Effect.of_deferred_fun (fun query ->
                Async_js.Rpc.Rpc.dispatch_exn (Rpc_repr.rpc repr) conn query)
            |> unstage
          in
          f query)
    }
  ;;

  module Handle_rpc = struct
    type ('conn_state, 'q, 'r) handler =
      | Plain of ('conn_state -> 'q -> 'r)
      | Async of ('conn_state -> 'q -> 'r Deferred.t)

    type 'conn_state t =
      { f : 'q 'r. ('q, 'r) Rpc_repr.t -> ('conn_state, 'q, 'r) handler }
  end

  let implementations reprs handle_rpc =
    List.map reprs ~f:(fun (Pack repr) ->
        match handle_rpc.Handle_rpc.f repr with
        | Plain handler -> Rpc.Rpc.implement' (Rpc_repr.rpc repr) handler
        | Async handler -> Rpc.Rpc.implement (Rpc_repr.rpc repr) handler)
  ;;

  module For_testing = struct
    let create_server conn_state handle_rpc =
      { Call_rpc.f =
          (fun (type q r) (repr : (q, r) Rpc_repr.t) (query : q) ->
            let f =
              (match handle_rpc.Handle_rpc.f repr with
              | Plain handler -> Bonsai_web.Effect.of_sync_fun (handler conn_state)
              | Async handler -> Bonsai_web.Effect.of_deferred_fun (handler conn_state))
              |> unstage
            in
            f query)
      }
    ;;
  end
end
