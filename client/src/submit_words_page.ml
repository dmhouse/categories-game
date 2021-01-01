open! Core_kernel
open Import

module Model = struct
  type t = Word.t option Category_id.Map.t [@@deriving sexp, compare, equal]

  let initial = Category_id.Map.empty
end

let view (model : Model.t) ~set_word ~(round_params : Round_params.t) ~time_remaining =
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
             let response = Map.find model category |> Option.join in
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
                       ; Attr.on_input (fun _ev word -> set_word word ~category)
                       ]
                       []
                   ]
               ]))
    ]
;;

let bonsai ~round_params ~clock ~submit_words ~time_remaining =
  let%sub model, set_model =
    Bonsai.state [%here] (module Model) ~default_model:Model.initial
  in
  let%sub () =
    Bonsai.Edge.lifecycle
      ~on_activate:
        (let%map set_model = set_model in
         set_model Model.initial)
      ()
  in
  let%sub () =
    Clock.every
      clock
      (Time_ns.Span.of_ms 250.)
      ~f:
        (let%map model = model
         and submit_words = submit_words in
         fun _now -> Bonsai.Effect.inject_ignoring_response (submit_words model))
  in
  Bonsai.read
    (let%map model = model
     and round_params = round_params
     and time_remaining = time_remaining
     and set_word =
       let%map set_model = set_model
       and model = model in
       fun word ~category ->
         let word = if String.is_empty word then None else Some (Word.of_string word) in
         set_model (Map.set model ~key:category ~data:word)
     in
     view model ~set_word ~round_params ~time_remaining)
;;
