open Core
open Async
open Import

module Conn_state = struct
  type t' =
    | Awaiting_login
    | Logged_in of
        { player : Player_id.t
        ; game_state : Game_state.t
        }

  type t = t' ref

  let create () = ref Awaiting_login
end

type t =
  { games : Game_state.t Game_id.Table.t
  ; rstate : Random.State.t
  ; time_source : Time_source.t
  }

let create rstate time_source = { games = Game_id.Table.create (); rstate; time_source }

let accept_login t ~game_id ~player =
  let game_state =
    Hashtbl.find_or_add t.games game_id ~default:(fun () ->
        let game_state = Game_state.create t.rstate ~id:game_id ~owner:player in
        print_s
          [%message "Created new game" (game_id : Game_id.t) (game_state : Game_state.t)];
        game_state)
  in
  print_s [%message "Adding player to game" (game_id : Game_id.t) (player : Player_id.t)];
  Game_state.add_player game_state player;
  game_state
;;

let log_in
    t
    (conn_state : Conn_state.t)
    ({ Rpc_protocol.Log_in.player; game_id; password } as query)
  =
  print_s [%message "login attempt" (query : Rpc_protocol.Log_in.query)];
  if String.( <> ) password "HoHoHo"
  then Or_error.error_string "wrong password"
  else (
    let game_state = accept_login t ~game_id ~player in
    let you_are_the_owner = Player_id.( = ) (Game_state.owner game_state) player in
    conn_state := Logged_in { player; game_state };
    Ok { Rpc_protocol.Log_in.you_are_the_owner })
;;

let get_game_status _t (conn_state : Conn_state.t) () =
  match !conn_state with
  | Awaiting_login -> error_s [%message "Log in first"]
  | Logged_in { player = _; game_state } -> Ok (Game_state.status game_state)
;;

let submit_words _t (conn_state : Conn_state.t) words =
  match !conn_state with
  | Awaiting_login -> error_s [%message "Log in first"]
  | Logged_in { player; game_state } -> Game_state.submit_words game_state player words
;;

let control_game t (conn_state : Conn_state.t) (action : Rpc_protocol.Control_game.query) =
  match !conn_state with
  | Awaiting_login -> error_s [%message "Log in first"]
  | Logged_in { player; game_state } ->
    let owner = Game_state.owner game_state in
    if Player_id.( <> ) player owner
    then error_s [%message "You are not the owner" (owner : Player_id.t)]
    else (
      match action with
      | Next_round -> Game_state.next_round game_state
      | Set_word_status { player; category; status } ->
        Game_state.set_word_status game_state ~player ~category ~status
      | Advance_results_presentation direction ->
        Game_state.advance_results_presentation game_state direction
      | Start_round { round_params } ->
        Game_state.start_round
          game_state
          ~round_params
          ~now:(Time_source.now t.time_source)
        |> Result.map ~f:(fun end_at ->
               upon (Time_source.at t.time_source end_at) (fun () ->
                   Game_state.end_round game_state)))
;;

let implementations t =
  Rpc.Implementations.create_exn
    ~implementations:
      [ Rpc.Rpc.implement' Rpc_protocol.Log_in.rpc (log_in t)
      ; Rpc.Rpc.implement' Rpc_protocol.Submit_words.rpc (submit_words t)
      ; Rpc.Rpc.implement' Rpc_protocol.Get_game_status.rpc (get_game_status t)
      ; Rpc.Rpc.implement' Rpc_protocol.Control_game.rpc (control_game t)
      ]
    ~on_unknown_rpc:`Raise
;;
