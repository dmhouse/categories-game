open! Core_kernel
open Import

let view
    ~(player_kind : Player_kind.t)
    ~presentation_stage
    ~(scores : Round_results.t)
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
        | Owner { control_game } ->
          Node.input
            [ Attr.type_ "submit"
            ; Attr.value "Next round"
            ; Attr.on_click (fun _ev ->
                  control_game Next_round |> Bonsai.Effect.inject_ignoring_response)
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
                     | Owner { control_game } ->
                       (match word with
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
                                       [ control_game
                                           (Set_word_status
                                              { player; category; status = set_to_status })
                                         |> Bonsai.Effect.inject_ignoring_response
                                       ; Event.Prevent_default
                                       ])
                               ]
                               [ Node.text text ]
                           ])
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
    ((Node.h2 [] [ Node.text "Results" ]
     ::
     (match player_kind with
     | Non_owner -> []
     | Owner { control_game } ->
       [ Node.input
           [ Attr.type_ "submit"
           ; Attr.value "Next category"
           ; Attr.on_click (fun _ev ->
                 control_game (Advance_results_presentation `Forward)
                 |> Bonsai.Effect.inject_ignoring_response)
           ]
           []
       ; Node.input
           [ Attr.type_ "submit"
           ; Attr.value "Previous category"
           ; Attr.on_click (fun _ev ->
                 control_game (Advance_results_presentation `Back)
                 |> Bonsai.Effect.inject_ignoring_response)
           ]
           []
       ]))
    @ results)
;;

let bonsai ~scores ~presentation_stage ~player_kind ~running_totals =
  (* Currently this is a pure function, but we wrap it up inside a computation
     to make it easier to add state later should we desire. *)
  Bonsai.read
    (let%map presentation_stage = presentation_stage
     and scores = scores
     and player_kind = player_kind
     and running_totals = running_totals in
     view ~presentation_stage ~scores ~player_kind ~running_totals)
;;
