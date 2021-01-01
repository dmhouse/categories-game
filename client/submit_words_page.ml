open! Core_kernel
open Import

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

let view (model : Model.t) ~inject ~(round_params : Round_params.t) ~time_remaining =
  let open Vdom in
  Node.div
    []
    [ Node.h2 [] [ Node.text "Get thinking!" ]
    ; Node.p
        []
        [ Node.text "Answers must begin with: "
        ; Node.strong [] [ Node.text (Char.to_string round_params.letter) ]
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
        (List.map (Set.to_list round_params.categories) ~f:(fun category ->
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

let bonsai ~round_params ~clock ~submit_words ~time_remaining =
  let%sub model, inject =
    Bonsai.state_machine0
      [%here]
      (module Model)
      (module Action)
      ~default_model:Model.initial
      ~apply_action
  in
  let%sub () =
    Clock.every
      clock
      (Time_ns.Span.of_ms 250.)
      ~f:
        (let%map model = model
         and submit_words = submit_words in
         fun _now ->
           Bonsai.Effect.inject_ignoring_response (submit_words model.Model.responses))
  in
  Bonsai.read
    (let%map model = model
     and inject = inject
     and round_params = round_params
     and time_remaining = time_remaining in
     view model ~inject ~round_params ~time_remaining)
;;
