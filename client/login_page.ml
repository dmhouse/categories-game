open! Core_kernel
open Import

module Model = struct
  type t =
    { player_id : Player_id.t option
    ; password : string option
    ; game_id : Game_id.t option
    }
  [@@deriving sexp, compare, equal]

  let initial = { player_id = None; password = None; game_id = None }
end

module Action = struct
  type t =
    | Set_player_id of string
    | Set_password of string
    | Set_game_id of string
  [@@deriving sexp]
end

let apply_action ~inject:_ ~schedule_event:_ (model : Model.t) (action : Action.t) =
  match action with
  | Set_player_id player ->
    { model with
      player_id =
        (if String.is_empty player then None else Some (Player_id.of_string player))
    }
  | Set_password pw ->
    { model with password = (if String.is_empty pw then None else Some pw) }
  | Set_game_id game_id ->
    { model with
      game_id =
        (if String.is_empty game_id then None else Some (Game_id.of_string game_id))
    }
;;

let view (model : Model.t) ~inject ~log_in =
  let open Vdom in
  Node.div
    []
    [ Node.input
        [ Attr.type_ "text"
        ; Attr.placeholder "Your name"
        ; Attr.value
            (match model.player_id with
            | None -> ""
            | Some player_id -> Player_id.to_string player_id)
        ; Attr.on_input (fun _ev str -> inject (Action.Set_player_id str))
        ]
        []
    ; Node.input
        [ Attr.type_ "text"
        ; Attr.placeholder "Password"
        ; Attr.value
            (match model.password with
            | None -> ""
            | Some pw -> pw)
        ; Attr.on_input (fun _ev str -> inject (Action.Set_password str))
        ]
        []
    ; Node.input
        [ Attr.type_ "text"
        ; Attr.placeholder "Game ID"
        ; Attr.value
            (match model.game_id with
            | None -> ""
            | Some gid -> Game_id.to_string gid)
        ; Attr.on_input (fun _ev str -> inject (Action.Set_game_id str))
        ]
        []
    ; Node.input
        ([ Attr.type_ "submit"; Attr.value "Go" ]
        @
        match
          let%map.Option player_id = model.player_id
          and password = model.password
          and game_id = model.game_id in
          { Rpc_protocol.Log_in.player_id; password; game_id }
        with
        | None -> [ Attr.disabled ]
        | Some query ->
          [ Attr.on_click (fun _ev ->
                log_in query |> Bonsai.Effect.inject_ignoring_response)
          ])
        []
    ]
;;

let bonsai ~log_in =
  let%sub model, inject =
    Bonsai.state_machine0
      [%here]
      (module Model)
      (module Action)
      ~default_model:Model.initial
      ~apply_action
  in
  Bonsai.read
    (let%map model = model
     and inject = inject
     and log_in = log_in in
     view model ~inject ~log_in)
;;
