open Core
open Import

module Finished_status = struct
  type t =
    { scores : Round_results.t
    ; presentation_stages : Results_presentation_stage.t List_zipper.t
    ; words : Word.t option Category_id.Map.t Player_id.Map.t
    ; round_params : Round_params.t
    ; disallowed_words : Player_and_category.Set.t
    }
  [@@deriving sexp_of]
end

type status =
  | Not_yet_started
  | Started of
      { end_at : Time_ns.t
      ; words : Word.t option Category_id.Map.t Player_id.Map.t
      ; round_params : Round_params.t
      }
  | Finished of Finished_status.t
[@@deriving sexp_of]

type t =
  { id : Game_id.t
  ; owner : Player_id.t
  ; mutable running_totals_ex_this_round : int Player_id.Map.t
  ; mutable categories_used_so_far : Category_id.Set.t
  ; mutable status : status
  ; rstate : (Random.State.t[@sexp.opaque])
  }
[@@deriving sexp_of, fields]

let add_player t player_id =
  t.running_totals_ex_this_round
    <- Map.set t.running_totals_ex_this_round ~key:player_id ~data:0
;;

let create rstate ~owner ~id =
  { id
  ; owner
  ; running_totals_ex_this_round = Player_id.Map.empty
  ; categories_used_so_far = Category_id.Set.empty
  ; status = Not_yet_started
  ; rstate
  }
;;

let players t = Set.of_map_keys t.running_totals_ex_this_round

let start_round t ~now ~(round_params : Round_params.t) =
  match t.status with
  | Finished _ | Started _ -> error_s [%message "Game not in pre-round state"]
  | Not_yet_started ->
    let end_at = Time_ns.add now round_params.length in
    t.status <- Started { round_params; end_at; words = Player_id.Map.empty };
    Ok end_at
;;

let submit_words t player words =
  match t.status with
  | Not_yet_started | Finished _ -> error_s [%message "Game not in play"]
  | Started { words = words_by_player; end_at; round_params } ->
    t.status
      <- Started
           { end_at
           ; round_params
           ; words = Map.set words_by_player ~key:player ~data:words
           };
    Ok ()
;;

let end_round t =
  match t.status with
  | Not_yet_started | Finished _ -> ()
  | Started { words; round_params; _ } ->
    let disallowed_words =
      Map.fold
        words
        ~init:Player_and_category.Set.empty
        ~f:(fun ~key:player ~data:words set ->
          Map.fold words ~init:set ~f:(fun ~key:category ~data:word set ->
              match word with
              | None -> set
              | Some word ->
                let word_str = Word.to_string word in
                let is_allowed =
                  if String.is_empty word_str
                  then false
                  else
                    String.Caseless.( = )
                      (Char.to_string word_str.[0])
                      (Char.to_string round_params.letter)
                in
                if is_allowed then set else Set.add set { player; category }))
    in
    let scores =
      Round_results.compute
        ~player_responses:words
        ~categories:(Set.to_list round_params.categories)
        ~disallowed_words
    in
    let presentation_stages =
      List.map (Category_id.Set.to_list round_params.categories) ~f:(fun category ->
          Results_presentation_stage.Category category)
      @ [ Total_scores ]
    in
    t.status
      <- Finished
           { scores
           ; presentation_stages = List_zipper.create presentation_stages
           ; words
           ; round_params
           ; disallowed_words
           }
;;

let advance_results_presentation t direction =
  match t.status with
  | Not_yet_started | Started _ -> error_s [%message "Game not finished yet"]
  | Finished finished ->
    let presentation_stages =
      match direction with
      | `Forward -> List_zipper.next finished.presentation_stages
      | `Back -> List_zipper.prev finished.presentation_stages
    in
    t.status <- Finished { finished with presentation_stages };
    Ok ()
;;

let set_word_status t ~player ~category ~(status : Word_status.t) =
  match t.status with
  | Not_yet_started | Started _ -> error_s [%message "Game not finished yet"]
  | Finished finished ->
    let player_and_category = { Player_and_category.player; category } in
    let disallowed_words =
      match status with
      | Allowed -> Set.remove finished.disallowed_words player_and_category
      | Disallowed -> Set.add finished.disallowed_words player_and_category
    in
    let scores =
      Round_results.compute
        ~player_responses:finished.words
        ~categories:(Set.to_list finished.round_params.categories)
        ~disallowed_words
    in
    t.status <- Finished { finished with scores; disallowed_words };
    Ok ()
;;

let running_totals t (finished : Finished_status.t) =
  Map.merge
    t.running_totals_ex_this_round
    finished.scores.total_scores
    ~f:(fun ~key:_ merge ->
      match merge with
      | `Left l -> Some l
      | `Right _ -> None
      | `Both (l, r) -> Some (l + r))
;;

let next_round t =
  match t.status with
  | Not_yet_started | Started _ -> error_s [%message "Game not finished yet"]
  | Finished finished ->
    t.running_totals_ex_this_round <- running_totals t finished;
    t.categories_used_so_far
      <- Set.union t.categories_used_so_far finished.round_params.categories;
    t.status <- Not_yet_started;
    Ok ()
;;

let status t =
  match t.status with
  | Not_yet_started ->
    Game_status.Pre_round
      { players = players t; categories_used_so_far = t.categories_used_so_far }
  | Started { end_at; round_params; words = _ } ->
    let time_remaining =
      Time_ns.diff end_at (Time_ns.now ()) |> Time_ns.Span.max Time_ns.Span.zero
    in
    In_play { time_remaining; round_params }
  | Finished finished ->
    Results_presentation
      { scores = finished.scores
      ; presentation_stage = List_zipper.focus finished.presentation_stages
      ; running_totals = running_totals t finished
      ; round_params = finished.round_params
      }
;;
