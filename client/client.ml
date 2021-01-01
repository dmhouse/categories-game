open! Core_kernel
open Virtual_dom
module Bonsai = Bonsai_web.Bonsai
open Bonsai.Let_syntax
open Scattegories_common

module Words = struct
  type t = Word.t option Category_id.Map.t
end

module Clock = struct
  module Time_ns_with_equal = struct
    type t = Time_ns.Stable.Alternate_sexp.V1.t [@@deriving sexp, compare]

    let equal = [%compare.equal: t]
  end

  type t = Time_ns_with_equal.t Bonsai.Var.t

  let create ~now = Bonsai.Var.create now
  let advance t ~now = Bonsai.Var.set t now

  let every_tick t ~callback =
    Bonsai.Edge.on_change
      [%here]
      (module Time_ns_with_equal)
      (Bonsai.Var.value t)
      ~callback
  ;;
end

module Login_page = struct
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
          | Some query -> [ Attr.on_click (fun _ev -> log_in query) ])
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
end

type player_kind =
  | Owner of
      { start_round : Game_info.t -> unit Bonsai.Effect.t
      ; advance_results_presentation : [`Forward|`Back] ->unit Bonsai.Effect.t
      ; next_round : unit Bonsai.Effect.t
      ; set_word_status :
          Word_status.t
          -> player:Player_id.t
          -> category:Category_id.t
          -> unit Bonsai.Effect.t
      }
  | Non_owner

module Start_round_page = struct
  let random_letter rstate = Char.of_int_exn (Char.to_int 'A' + Random.State.int rstate 26)

  let random_categories rstate ~categories_used_so_far =
    let available_categories = Set.diff Categories.all categories_used_so_far in
    let random_element_and_rest set =
      let n = Random.State.int rstate (Set.length set) in
      Option.value_exn (Set.nth set n), Set.remove_index set n
    in
    let rec loop ~choose_from ~chosen num_choices =
      if num_choices <= 0
      then chosen
      else (
        let choice, rest = random_element_and_rest choose_from in
        loop ~choose_from:rest ~chosen:(Set.add chosen choice) (num_choices - 1))
    in
    loop ~choose_from:available_categories ~chosen:Category_id.Set.empty 12
    |> Set.to_list
    |> List.permute ~random_state:rstate
  ;;

  module Model = struct
    type t =
      { letter : Char.t
      ; categories : Category_id.t list
      ; game_length : Time_ns.Span.t
      }
    [@@deriving sexp, compare, equal]

    let game_info t ~id =
      { Game_info.id
      ; letter = t.letter
      ; length = t.game_length
      ; categories = Category_id.Set.of_list t.categories
      }
    ;;

    let initial rstate =
      { categories =
          random_categories rstate ~categories_used_so_far:Category_id.Set.empty
      ; letter = random_letter rstate
      ; game_length = Time_ns.Span.of_sec 120.
      }
    ;;
  end

  module Action = struct
    type t =
      | Set_letter of string
      | Set_categories of string
      | Set_game_length of string
      | Randomise_letter
      | Randomise_categories
    [@@deriving sexp, compare, equal]
  end

  let apply_action
      rstate
      ~inject:_
      ~schedule_event:_
      categories_used_so_far
      (model : Model.t)
      (action : Action.t)
    =
    match action with
    | Set_letter str ->
      if String.length str = 1 && Char.( >= ) str.[0] 'A' && Char.( <= ) str.[0] 'Z'
      then { model with letter = str.[0] }
      else model
    | Set_categories str ->
      let categories =
        String.split str ~on:'\n'
        |> List.filter_map ~f:(fun str ->
               if String.is_empty str then None else Some (Category_id.of_string str))
      in
      { model with categories }
    | Set_game_length str ->
      (match Float.of_string str with
      | exception _ -> model
      | secs -> { model with game_length = Time_ns.Span.of_sec secs })
    | Randomise_letter -> { model with letter = random_letter rstate }
    | Randomise_categories ->
      { model with categories = random_categories rstate ~categories_used_so_far }
  ;;

  let view (model : Model.t) ~player_kind ~players ~game_id ~(inject : Action.t -> _) =
    let open Vdom in
    Node.div
      []
      ([ Node.h2
           []
           [ Node.text
               (match player_kind with
               | Owner _ -> "Game setup"
               | Non_owner -> "Waiting to start")
           ]
       ; Node.p
           []
           [ Node.text "Current players: "
           ; Node.text
               (Set.to_list players
               |> List.map ~f:Player_id.to_string
               |> String.concat ~sep:", ")
           ]
       ]
      @
      match player_kind with
      | Non_owner ->
        [ Node.p [] [ Node.text "Waiting for the owner to start the game..." ] ]
      | Owner { start_round; _ } ->
        [ Node.h3 [] [ Node.text "Letter" ]
        ; Node.div
            []
            [ Node.select
                [ Attr.on_change (fun _ev value -> inject (Set_letter value)) ]
                (List.init 26 ~f:(fun letter_idx ->
                     let letter = Char.of_int_exn (Char.to_int 'A' + letter_idx) in
                     Node.option
                       (Attr.value (Char.to_string letter)
                       ::
                       (if Char.( = ) letter model.letter then [ Attr.selected ] else [])
                       )
                       [ Node.text (Char.to_string letter) ]))
            ; Node.input
                [ Attr.type_ "submit"
                ; Attr.value "Randomise"
                ; Attr.on_click (fun _ev -> inject Randomise_letter)
                ]
                []
            ]
        ; Node.h3 [] [ Node.text "Categories" ]
        ; Node.div
            []
            [ Node.textarea
                [ Attr.create "rows" "15"
                ; Attr.style (Css_gen.width (`Percent (Percent.of_mult 0.8)))
                ; Attr.on_input (fun _ev value -> inject (Set_categories value))
                ]
                [ Node.text
                    (List.map model.categories ~f:Category_id.to_string
                    |> String.concat ~sep:"\n")
                ]
            ; Node.input
                [ Attr.type_ "submit"
                ; Attr.value "Randomise"
                ; Attr.on_click (fun _ev -> inject Randomise_categories)
                ]
                []
            ]
        ; Node.h3 [] [ Node.text "Game length" ]
        ; Node.div
            []
            [ Node.input
                [ Attr.type_ "text"
                ; Attr.value
                    (Time_ns.Span.to_sec model.game_length
                    |> Float.to_string_hum ~strip_zero:true)
                ; Attr.on_input (fun _ev str -> inject (Set_game_length str))
                ]
                []
            ; Node.text " seconds"
            ]
        ; Node.input
            [ Attr.type_ "submit"
            ; Attr.value "Start game"
            ; Attr.on_click (fun _ev ->
                  start_round (Model.game_info model ~id:game_id)
                  |> Bonsai.Effect.inject_ignoring_response)
            ]
            []
        ])
  ;;

  let bonsai rstate ~player_kind ~players ~game_id ~categories_used_so_far =
    let%sub model, inject =
      Bonsai.state_machine1
        [%here]
        (module Model)
        (module Action)
        ~default_model:(Model.initial rstate)
        ~apply_action:(apply_action rstate)
        categories_used_so_far
    in
    let%sub () =
      Bonsai.Edge.lifecycle
        ~on_activate:
          (let%map inject = inject in
           Ui_event.Many [ inject Randomise_letter; inject Randomise_categories ])
        ()
    in
    Bonsai.read
      (let%map player_kind = player_kind
       and players = players
       and inject = inject
       and model = model
       and game_id = game_id in
       view model ~player_kind ~players ~game_id ~inject)
  ;;
end

module Submit_results_page = struct
  module Model = struct
    type t = { responses : Word.t option Category_id.Map.t }
    [@@deriving sexp, compare, equal]

    let initial = { responses = Category_id.Map.empty }
  end

  module Action = struct
    type t =
      | Set_response of
          { category : Category_id.t
          ; response : string
          }
    [@@deriving sexp, compare, equal]
  end

  let apply_action ~inject:_ ~schedule_event:_ (model : Model.t) (action : Action.t) =
    match action with
    | Set_response { category; response } ->
      let word =
        if String.is_empty response then None else Some (Word.of_string response)
      in
      { Model.responses = Map.set model.responses ~key:category ~data:word }
  ;;

  let view (model : Model.t) ~inject ~(game_info : Game_info.t) ~time_remaining =
    let open Vdom in
    Node.div
      []
      [ Node.h2 [] [ Node.text "Get thinking!" ]
      ; Node.p
          []
          [ Node.text "Answers must begin with: "
          ; Node.strong [] [ Node.text (Char.to_string game_info.letter) ]
          ]
      ; Node.div
          [ Attr.id "time-remaining" ]
          [ Node.strong [] [ Node.text "Time remaining: " ]
          ; Node.text
              (sprintf
                 "%ds"
                 (Time_ns.Span.to_sec time_remaining |> Float.iround_nearest_exn))
          ]
      ; Node.table
          [ Attr.id "responses" ]
          (List.map (Set.to_list game_info.categories) ~f:(fun category ->
               let response = Map.find model.responses category |> Option.join in
               Node.tr
                 []
                 [ Node.th [] [ Node.text (Category_id.to_string category) ]
                 ; Node.td
                     []
                     [ Node.input
                         [ Attr.value
                             (match response with
                             | None -> ""
                             | Some word -> Word.to_string word)
                         ; Attr.type_ "text"
                         ; Attr.on_input (fun _ev response ->
                               inject (Action.Set_response { category; response }))
                         ]
                         []
                     ]
                 ]))
      ]
  ;;

  let bonsai ~game_info ~clock ~submit_words ~time_remaining =
    let%sub model, inject =
      Bonsai.state_machine0
        [%here]
        (module Model)
        (module Action)
        ~default_model:Model.initial
        ~apply_action
    in
    let%sub () =
      Clock.every_tick
        clock
        ~callback:
          (let%map model = model
           and submit_words = submit_words in
           fun _now ->
             Bonsai.Effect.inject_ignoring_response (submit_words model.Model.responses))
    in
    Bonsai.read
      (let%map model = model
       and inject = inject
       and game_info = game_info
       and time_remaining = time_remaining in
       view model ~inject ~game_info ~time_remaining)
  ;;
end

module Review_results_page = struct
  let view
      ~player_id:_
      ~player_kind
      ~presentation_stage
      ~(scores : Game_results.t)
      ~running_totals
    =
    let open Vdom in
    let results =
      match presentation_stage with
      | None -> []
      | Some Results_presentation_stage.Total_scores ->
        [ Node.h3 [] [ Node.text "Total scores" ]
        ; Node.table
            [ Attr.id "player-scores" ]
            (Node.tr
               []
               [ Node.th [] [ Node.text "Player" ]
               ; Node.th [] [ Node.text "This round" ]
               ; Node.th [] [ Node.text "Total" ]
               ]
            :: List.map (Map.to_alist scores.total_scores) ~f:(fun (player, score) ->
                   Node.tr
                     []
                     [ Node.th [] [ Node.text (Player_id.to_string player) ]
                     ; Node.td [] [ Node.text (Int.to_string score) ]
                     ; Node.td
                         []
                         [ Node.text
                             (match Map.find running_totals player with
                             | None -> ""
                             | Some total -> Int.to_string total)
                         ]
                     ]))
        ; (match player_kind with
          | Non_owner -> Node.none
          | Owner { next_round; _ } ->
            Node.input
              [ Attr.type_ "submit"
              ; Attr.value "Next round"
              ; Attr.on_click (fun _ev ->
                    Bonsai.Effect.inject_ignoring_response next_round)
              ]
              [])
        ]
      | Some (Category category) ->
        (match Map.find scores.by_category category with
        | None -> []
        | Some by_player ->
          [ Node.h3 [] [ Node.text (Category_id.to_string category) ]
          ; Node.table
              []
              (Node.tr
                 []
                 [ Node.th [] [ Node.text "Player" ]
                 ; Node.th [] [ Node.text "Word" ]
                 ; Node.th [] [ Node.text "Score" ]
                 ; (match player_kind with
                   | Non_owner -> Node.none
                   | Owner _ -> Node.th [] [])
                 ]
              :: List.map
                   (Map.to_alist by_player)
                   ~f:(fun (player, { word; status; score }) ->
                     let word_cell =
                       match word with
                       | None -> Node.text "-"
                       | Some word ->
                         let strikethrough_attr =
                           match status with
                           | Allowed -> None
                           | Disallowed ->
                             Some
                               (Attr.style
                                  (Css_gen.text_decoration ~line:[ `Line_through ] ()))
                         in
                         Node.span
                           (Option.to_list strikethrough_attr)
                           [ Node.text (Word.to_string word) ]
                     in
                     let set_word_status_cell =
                       match player_kind with
                       | Non_owner -> Node.none
                       | Owner { set_word_status; _ } ->
                          match word with
                          | None -> Node.td [] []
                          | Some _ ->
                         let set_to_status, text =
                           match status with
                           | Allowed -> Word_status.Disallowed, "disallow"
                           | Disallowed -> Allowed, "allow"
                         in
                         Node.td
                           []
                           [ Node.a
                               [ Attr.href "#"
                               ; Attr.on_click (fun _ev ->
                                     Event.Many
                                       [ Bonsai.Effect.inject_ignoring_response
                                           (set_word_status
                                              set_to_status
                                              ~player
                                              ~category)
                                       ; Event.Prevent_default
                                       ])
                               ]
                               [ Node.text text ]
                           ]
                     in
                     Node.tr
                       []
                       [ Node.td [] [ Node.text (Player_id.to_string player) ]
                       ; Node.td [] [ word_cell ]
                       ; Node.td [] [ Node.text (Int.to_string score) ]
                       ; set_word_status_cell
                       ]))
          ])
    in
    Node.div
      []
      (( Node.h2 [] [ Node.text "Results" ] )
       ::
        (match player_kind with
         | Non_owner -> []
         | Owner { advance_results_presentation; _ } ->
            [
           Node.input
             [ Attr.type_ "submit"
             ; Attr.value "Next category"
             ; Attr.on_click (fun _ev ->
                   advance_results_presentation `Forward
                             |> Bonsai.Effect.inject_ignoring_response )
             ]
             []
           ;
             Node.input
               [ Attr.type_ "submit"
               ; Attr.value "Previous category"
               ; Attr.on_click (fun _ev ->
                     advance_results_presentation `Back
                     |> Bonsai.Effect.inject_ignoring_response )
               ]
               []
            ]
         )
      @ results)
  ;;

  let view ~scores ~presentation_stage ~player_kind ~player_id ~running_totals =
    let%map presentation_stage = presentation_stage
    and scores = scores
    and player_kind = player_kind
    and player_id = player_id
    and running_totals = running_totals in
    view ~presentation_stage ~scores ~player_kind ~player_id ~running_totals
  ;;
end

module Model = struct
  type t' =
    { player_id : Player_id.t
    ; game_id : Game_id.t
    ; game_status : Game_status.t option
    ; am_owner : bool
    }
  [@@deriving sexp, compare, equal]

  type t =
    | Awaiting_login
    | Logged_in of t'
  [@@deriving sexp, compare, equal]
end

let bonsai
    rstate
    ~log_in_effect
    ~start_round_effect
    ~advance_results_presentation
    ~next_round
    ~get_game_status
    ~set_word_status
    ~submit_words
    ~clock
  =
  let%sub model, set_model =
    Bonsai.state [%here] (module Model) ~default_model:Awaiting_login
  in
  let%sub view =
    match%sub model with
    | Awaiting_login ->
      let log_in =
        let%map set_model = set_model in
        fun query ->
          Bonsai.Effect.inject (log_in_effect query) ~on_response:(function
              | Error _ -> Bonsai.Event.Ignore
              | Ok { Rpc_protocol.Log_in.you_are_the_owner } ->
                set_model
                  (Model.Logged_in
                     { player_id = query.Rpc_protocol.Log_in.player_id
                     ; game_id = query.game_id
                     ; game_status = None
                     ; am_owner = you_are_the_owner
                     }))
      in
      Login_page.bonsai ~log_in
    | Logged_in { player_id; game_id; game_status; am_owner } ->
      let%sub () =
        Clock.every_tick
          clock
          ~callback:
            (let%map game_id = game_id
             and player_id = player_id
             and set_model = set_model
             and am_owner = am_owner in
             fun _now ->
               get_game_status { Rpc_protocol.Get_game_status.game_id; player_id }
               |> Bonsai.Effect.inject ~on_response:(fun game_status ->
                      set_model
                        (Logged_in
                           { game_id
                           ; player_id
                           ; game_status = Some game_status
                           ; am_owner
                           })))
      in
      let player_kind =
        let%map game_id = game_id
        and am_owner = am_owner in
        if am_owner
        then
          Owner
            { start_round =
                (fun game_info ->
                  start_round_effect { Rpc_protocol.Start_round.game_id; game_info })
            ; advance_results_presentation =
                (fun direction ->
                advance_results_presentation { Rpc_protocol.Advance_results_presentation.game_id; direction })
            ; next_round = next_round { Rpc_protocol.Next_round.game_id }
            ; set_word_status =
                (fun status ~player ~category ->
                  set_word_status
                    { Rpc_protocol.Set_word_status.game_id; status; player; category })
            }
        else Non_owner
      in
      let computation =
        match%sub game_status with
        | None -> Bonsai.const (Vdom.Node.text "Loading...")
        | Some (Not_yet_started { players; categories_used_so_far }) ->
          Start_round_page.bonsai
            rstate
            ~players
            ~player_kind
            ~game_id
            ~categories_used_so_far
        | Some (In_play { time_remaining; game_info }) ->
          let submit_words =
            let%map player_id = player_id
            and game_info = game_info in
            fun words ->
              submit_words
                { Rpc_protocol.Submit_words.game_id = game_info.id; player_id; words }
          in
          Submit_results_page.bonsai ~game_info ~clock ~submit_words ~time_remaining
        | Some
            (Game_finished { scores; presentation_stage; running_totals; game_info = _ })
          ->
          Bonsai.read
            (Review_results_page.view
               ~scores
               ~presentation_stage
               ~player_kind
               ~player_id
               ~running_totals)
      in
      let%sub result, reset_model = Bonsai.with_model_resetter computation in
      let%sub () =
        Bonsai.Edge.on_change'
          [%here]
          (module struct
            type t = Game_status.t option [@@deriving sexp, equal]
          end)
          game_status
          ~callback:
            (let%map reset_model = reset_model in
             fun old new_ ->
               let should_reset =
                 match old with
                 | None -> true
                 | Some old ->
                   (match old, new_ with
                   | ( Some (Game_status.Not_yet_started _)
                     , Some (Game_status.Not_yet_started _) )
                   | Some (In_play _), Some (In_play _)
                   | Some (Game_finished _), Some (Game_finished _) -> false
                   | _ -> true)
               in
               if should_reset then reset_model else Vdom.Event.Ignore)
      in
      Bonsai.read result
  in
  Bonsai.read
    (let%map view = view in
     Vdom.Node.div [ Vdom.Attr.id "container" ] [ view ])
;;

let go () =
  let open Async_kernel in
  let%bind conn = Async_js.Rpc.Connection.client_exn () in
  let log_in_effect =
    Bonsai_web.Effect.of_deferred_fun (fun query ->
        Async_js.Rpc.Rpc.dispatch_exn Rpc_protocol.Log_in.rpc conn query)
    |> unstage
  in
  let start_round_effect =
    Bonsai_web.Effect.of_deferred_fun (fun query ->
        Async_js.Rpc.Rpc.dispatch_exn Rpc_protocol.Start_round.rpc conn query)
    |> unstage
  in
  let advance_results_presentation =
    Bonsai_web.Effect.of_deferred_fun (fun query ->
        Async_js.Rpc.Rpc.dispatch_exn Rpc_protocol.Advance_results_presentation.rpc conn query
        >>| ok_exn)
    |> unstage
  in
  let next_round =
    Bonsai_web.Effect.of_deferred_fun (fun query ->
        Async_js.Rpc.Rpc.dispatch_exn Rpc_protocol.Next_round.rpc conn query >>| ok_exn)
    |> unstage
  in
  let submit_words =
    Bonsai_web.Effect.of_deferred_fun (fun query ->
        Async_js.Rpc.Rpc.dispatch_exn Rpc_protocol.Submit_words.rpc conn query >>| ok_exn)
    |> unstage
  in
  let get_game_status =
    Bonsai_web.Effect.of_deferred_fun (fun query ->
        Async_js.Rpc.Rpc.dispatch_exn Rpc_protocol.Get_game_status.rpc conn query
        >>| ok_exn)
    |> unstage
  in
  let set_word_status =
    Bonsai_web.Effect.of_deferred_fun (fun query ->
        Async_js.Rpc.Rpc.dispatch_exn Rpc_protocol.Set_word_status.rpc conn query
        >>| ok_exn)
    |> unstage
  in
  let clock = Clock.create ~now:(Time_ns.now ()) in
  every (Time_ns.Span.of_sec 0.1) (fun () -> Clock.advance clock ~now:(Time_ns.now ()));
  let handle =
    Bonsai_web.Start.start
      Bonsai_web.Start.Result_spec.just_the_view
      ~bind_to_element_with_id:"app"
      (bonsai
         (Random.State.make_self_init ())
         ~log_in_effect
         ~start_round_effect
         ~submit_words
         ~advance_results_presentation
         ~next_round
         ~get_game_status
         ~set_word_status
         ~clock)
  in
  ignore handle;
  Deferred.unit
;;

let () =
  Async_js.init ();
  Async_kernel.don't_wait_for (go ())
;;
