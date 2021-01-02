open! Core_kernel
open Import

module Model = struct
  type t' =
    { player : Player_id.t
    ; game_status : Game_status.t option
    ; am_owner : bool
    }
  [@@deriving sexp, compare, equal]

  type t =
    | Awaiting_login
    | Logged_in of t'
  [@@deriving sexp, compare, equal]
end

let bonsai rstate clock ~(call_rpc : Rpc_protocol.Which.Call_rpc.t) ~on_error =
  let%sub model, set_model =
    Bonsai.state [%here] (module Model) ~default_model:Awaiting_login
  in
  let%sub view =
    match%sub model with
    | Awaiting_login ->
      let log_in =
        let%map set_model = set_model in
        fun query ->
          let open Bonsai.Effect.Let_syntax in
          match%bind call_rpc.f Log_in query >>| Or_error.join with
          | Error _ -> return ()
          | Ok { Rpc_protocol.Log_in.you_are_the_owner } ->
            set_model
              (Model.Logged_in
                 { player = query.Rpc_protocol.Log_in.player
                 ; game_status = None
                 ; am_owner = you_are_the_owner
                 })
            |> Bonsai.Effect.of_event
      in
      Login_page.bonsai ~log_in
    | Logged_in { player; game_status; am_owner } ->
      let%sub () =
        Clock.every
          clock
          (Time_ns.Span.of_ms 100.)
          ~f:
            (let%map player = player
             and set_model = set_model
             and am_owner = am_owner in
             fun _now ->
               call_rpc.f Get_game_status ()
               |> Bonsai.Effect.map ~f:Or_error.join
               |> Bonsai.Effect.inject ~on_response:(function
                      | Error e ->
                        on_error (Error.tag e ~tag:"Error getting game status");
                        Ui_event.Ignore
                      | Ok game_status ->
                        set_model
                          (Logged_in { player; game_status = Some game_status; am_owner })))
      in
      let player_kind =
        if%map am_owner
        then
          Player_kind.Owner
            { control_game =
                (fun action ->
                  let open Bonsai.Effect.Let_syntax in
match%map call_rpc.f Control_game action >>| Or_error.join with
                  | Ok () -> ()
                  | Error e -> on_error (Error.tag e ~tag:"Error controlling game"))
            }
        else Non_owner
      in
      (match%sub game_status with
      | None -> Bonsai.const (Vdom.Node.text "Loading...")
      | Some (Pre_round { players; categories_used_so_far }) ->
        Pre_round_page.bonsai rstate ~players ~player_kind ~categories_used_so_far
      | Some (In_play { time_remaining; round_params }) ->
        let submit_words =
          Bonsai.Value.return (fun words ->
              let open Bonsai.Effect.Let_syntax in
              match%map call_rpc.f Submit_words words >>| Or_error.join with
              | Ok () -> ()
              | Error e -> on_error (Error.tag e ~tag:"Error submitting words"))
        in
        Submit_words_page.bonsai ~round_params ~clock ~submit_words ~time_remaining
      | Some
          (Results_presentation
            { scores; presentation_stage; running_totals; round_params = _ }) ->
        Results_presentation_page.bonsai
          ~scores
          ~presentation_stage
          ~player_kind
          ~running_totals)
  in
  Bonsai.read
    (let%map view = view in
     Vdom.Node.div [ Vdom.Attr.id "container" ] [ view ])
;;

let go () =
  let open Async_kernel in
  Async_js.init ();
  don't_wait_for
  @@
  let uri =
    let open Js_of_ocaml in
    let scheme = if String.equal Url.Current.protocol "https:" then "wss" else "ws" in
    let port =
      match Url.Current.port with
      | Some port -> port
      | None ->
        if String.equal Url.Current.protocol "https:"
        then Url.default_https_port
        else Url.default_http_port
    in
    let host = Url.Current.host in
    let path = Url.Current.path_string in
    Uri.make ~scheme ~host ~port ~path ()
  in
  let%bind conn = Async_js.Rpc.Connection.client_exn ~uri () in
  let clock = Clock.create ~now:(Time_ns.now ()) in
  every (Time_ns.Span.of_sec 0.1) (fun () -> Clock.advance clock ~now:(Time_ns.now ()));
  let handle =
    Bonsai_web.Start.start
      Bonsai_web.Start.Result_spec.just_the_view
      ~bind_to_element_with_id:"app"
      (bonsai
         (Random.State.make_self_init ())
         clock
         ~call_rpc:(Rpc_protocol.Which.call_by_dispatching conn)
         ~on_error:(fun e -> Js_of_ocaml.Firebug.console##log (Error.to_string_hum e)))
  in
  ignore handle;
  Deferred.unit
;;

module For_testing = struct
  let bonsai = bonsai
end
