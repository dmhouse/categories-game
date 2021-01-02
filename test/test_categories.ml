open Core_kernel
open Async_kernel
open Categories_common

type t =
  { handle : (Vdom.Node.t, Nothing.t) Bonsai_web_test.Handle.t
  ; advance_clock : now:Time_ns.t -> unit Deferred.t
  }

let test f =
  let server_random_state = Random.State.init [| 1; 2; 3 |] in
  let gui_random_state = Random.State.init [| 4; 5; 6 |] in
  let time_source = Time_source.create ~now:Time_ns.epoch in
  let conn_state = Categories_server.Server_state.Conn_state.create () in
  let call_rpc (type q r) (rpc : (q, r) Rpc_protocol.Which.t) (query : q) : r =
    print_s [%message "RPC called" (rpc : (q, r) Rpc_protocol.Which.t)];
    Categories_server.Server_state.For_testing.handle_rpc rpc conn_state q
  in
  let call_rpc = { Categories_gui.For_testing.f = call_rpc } in
  let clock = Categories_gui.Clock.create ~now:(Time_source.now time_source) in
  let advance_clock ~now =
    Categories_gui.Clock.advance clock ~now;
    Time_source.advance_by_alarms time_source ~to_:now
  in
  let handle =
    Bonsai_web_test.Handle.create
      (Bonsai_web_test.Result_spec.vdom Fn.id)
      (Categories_gui.For_testing.bonsai gui_random_state clock ~call_rpc)
  in
  let server_state = Server_state.create t.rstate t.time_source in
  f { handle; advance_clock }
;;

let%expect_test _ =
  test (fun h -> Bonsai_test.Handle.show h);
  [%expect {||}]
;;
