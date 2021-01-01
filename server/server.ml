open Core
open Import
open Async

module Files = struct
  type t =
    { js : Filename.t
    ; css : Filename.t
    ; html : Filename.t
    }

  let param =
    let%map_open.Command js =
      flag "js" (required Filename.arg_type) ~doc:" path to javascript"
    and css = flag "css" (required Filename.arg_type) ~doc:" path to css"
    and html = flag "html" (required Filename.arg_type) ~doc:" path to html" in
    { js; css; html }
  ;;
end

let static_files_handler ~body:_ _sock (request : Cohttp.Request.t) ~(files : Files.t) =
  match request.resource with
  | "/scattegories.bc.js" -> Cohttp_async.Server.respond_with_file files.js
  | "/style.css" -> Cohttp_async.Server.respond_with_file files.css
  | "/" | "/index.html" -> Cohttp_async.Server.respond_with_file files.html
  | _ ->
    printf "404: %s\n" request.resource;
    Cohttp_async.Server.respond_string "Not found" ~status:`Not_found
;;

let categories _rstate =
  List.map ~f:Category_id.of_string [ "Sport"; "Country"; "Girl's name"; "Boy's name" ]
  |> Category_id.Set.of_list
;;

module State = struct
  type t =
    { games : Game_state.t Game_id.Table.t
    ; rstate : Random.State.t
    ; time_source : Time_source.t
    }

  let accept_login t ~game_id ~player_id =
    let game_state =
      Hashtbl.find_or_add t.games game_id ~default:(fun () ->
          let game_state = Game_state.create t.rstate ~id:game_id ~owner:player_id in
          print_s
            [%message
              "Created new game" (game_id : Game_id.t) (game_state : Game_state.t)];
          game_state)
    in
    print_s
      [%message "Adding player to game" (game_id : Game_id.t) (player_id : Player_id.t)];
    Game_state.add_player game_state player_id;
    game_state
  ;;

  let create rstate time_source = { games = Game_id.Table.create (); rstate; time_source }
end

let log_in
    (state : State.t)
    ()
    ({ Rpc_protocol.Log_in.player_id; game_id; password } as query)
  =
  print_s [%message "login attempt" (query : Rpc_protocol.Log_in.query)];
  if String.( <> ) password "HoHoHo"
  then Or_error.error_string "wrong password"
  else (
    let game_state = State.accept_login state ~game_id ~player_id in
    let you_are_the_owner = Player_id.( = ) (Game_state.owner game_state) player_id in
    Ok { Rpc_protocol.Log_in.you_are_the_owner })
;;

let get_game_status
    (state : State.t)
    ()
    { Rpc_protocol.Get_game_status.game_id; player_id = _ }
  =
  match Hashtbl.find state.games game_id with
  | None -> error_s [%message "Unknown game" (game_id : Game_id.t)]
  | Some game_state -> Ok (Game_state.status game_state)
;;

let submit_words
    (state : State.t)
    ()
    { Rpc_protocol.Submit_words.game_id; player_id; words }
  =
  match Hashtbl.find state.games game_id with
  | None -> error_s [%message "Unknown game" (game_id : Game_id.t)]
  | Some game_state -> Game_state.submit_words game_state player_id words
;;

let control_game (state : State.t) () { Rpc_protocol.Control_game.action; game_id } =
  match Hashtbl.find state.games game_id with
  | None -> error_s [%message "Unknown game" (game_id : Game_id.t)]
  | Some game_state ->
    (match action with
    | Next_round -> Game_state.next_round game_state
    | Set_word_status { player; category; status } ->
      Game_state.set_word_status game_state ~player ~category ~status
    | Advance_results_presentation direction ->
      Game_state.advance_results_presentation game_state direction
    | Start_round { round_params } ->
      Game_state.start_round
        game_state
        ~round_params
        ~now:(Time_source.now state.time_source)
      |> Result.map ~f:(fun end_at ->
             upon (Time_source.at state.time_source end_at) (fun () ->
                 Game_state.end_round game_state)))
;;

let implementations state =
  Rpc.Implementations.create_exn
    ~implementations:
      [ Rpc.Rpc.implement' Rpc_protocol.Log_in.rpc (log_in state)
      ; Rpc.Rpc.implement' Rpc_protocol.Submit_words.rpc (submit_words state)
      ; Rpc.Rpc.implement' Rpc_protocol.Get_game_status.rpc (get_game_status state)
      ; Rpc.Rpc.implement' Rpc_protocol.Control_game.rpc (control_game state)
      ]
    ~on_unknown_rpc:`Raise
;;

let go ~files ~rstate ~time_source =
  let state = State.create rstate time_source in
  let handler =
    Cohttp_async_websocket.Server.create
      ~opcode:`Binary
      ~non_ws_request:(static_files_handler ~files)
      (fun ~inet:_ ~subprotocol:_ _request ->
        Cohttp_async_websocket.Server.On_connection.create (fun reader writer ->
            let open Async_rpc_kernel in
            let transport = Pipe_transport.(create Kind.string reader writer) in
            let%bind conn =
              Rpc.Connection.create
                transport
                ~connection_state:(Fn.const ())
                ~implementations:(implementations state)
              >>| Result.ok_exn
            in
            let%bind () = Rpc.Connection.close_finished conn in
            Rpc.Transport.close transport)
        |> Deferred.return)
  in
  let%bind _server =
    Cohttp_async.Server.create_expert
      ~on_handler_error:`Ignore
      (Tcp.Where_to_listen.of_port 8000)
      handler
  in
  Deferred.never ()
;;

let () =
  Command.async
    ~summary:"start server"
    (let%map_open.Command files = Files.param in
     fun () ->
       go
         ~files
         ~rstate:(Random.State.make_self_init ())
         ~time_source:(Time_source.wall_clock ()))
  |> Command.run
;;
