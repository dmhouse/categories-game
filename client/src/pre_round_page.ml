open! Core_kernel
open Import

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

  let round_params t =
    { Round_params.letter = t.letter
    ; length = t.game_length
    ; categories = Category_id.Set.of_list t.categories
    }
  ;;

  let initial rstate =
    { categories = random_categories rstate ~categories_used_so_far:Category_id.Set.empty
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

let view
    (model : Model.t)
    ~(player_kind : Player_kind.t)
    ~players
    ~(inject : Action.t -> _)
  =
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
    | Owner { control_game } ->
      [ Node.h3 [] [ Node.text "Letter" ]
      ; Node.div
          []
          [ Node.select
              [ Attr.on_change (fun _ev value -> inject (Set_letter value)) ]
              (List.init 26 ~f:(fun letter_idx ->
                   let letter = Char.of_int_exn (Char.to_int 'A' + letter_idx) in
                   Node.option
                     (Attr.value (Char.to_string letter)
                     :: (if Char.( = ) letter model.letter then [ Attr.selected ] else [])
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
                control_game (Start_round { round_params = Model.round_params model })
                |> Bonsai.Effect.inject_ignoring_response)
          ]
          []
      ])
;;

let bonsai rstate ~player_kind ~players ~categories_used_so_far =
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
     and model = model in
     view model ~player_kind ~players ~inject)
;;
