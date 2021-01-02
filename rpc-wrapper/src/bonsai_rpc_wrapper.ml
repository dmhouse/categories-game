open Core_kernel
open Async_kernel
open Async_rpc_kernel
open Bonsai_rpc_wrapper_intf

module type Rpc_repr = Rpc_repr
module type S = S

module Make (Rpc_repr : Rpc_repr) = struct
  type packed_repr = Pack : ('q, 'r) Rpc_repr.t -> packed_repr

  module Call_rpc = struct
    type t = { f : 'q 'r. ('q, 'r) Rpc_repr.t -> 'q -> 'r Or_error.t Bonsai.Effect.t }
  end

  let call_by_dispatching conn =
    { Call_rpc.f =
        (fun (type q r) (repr : (q, r) Rpc_repr.t) (query : q) ->
          let f =
            Bonsai_web.Effect.of_deferred_fun (fun query ->
                Rpc.Rpc.dispatch (Rpc_repr.rpc repr) conn query)
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
    let create_server ?(should_log = Fn.const true) conn_state handle_rpc =
      { Call_rpc.f =
          (fun (type q r) (repr : (q, r) Rpc_repr.t) (query : q) ->
            if should_log (Pack repr)
            then (
              let rpc = Rpc_repr.rpc repr in
              print_s
                [%message
                  "Got RPC" ~name:(Rpc.Rpc.name rpc) ~version:(Rpc.Rpc.version rpc : int)]);
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
