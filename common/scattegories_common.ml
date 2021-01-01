open Core_kernel
open Async_js
module Category_id : Identifiable = String

module Word = struct
  include String.Caseless

  include Identifiable.Make_using_comparator (struct
    include String.Caseless

    let module_name = "Scattegories_common.Word"
    let to_string = Fn.id
    let of_string = Fn.id
  end)
end

module Player_id : Identifiable = String
module Game_id : Identifiable = String

module Round_params = struct
  type t =
    { id : Game_id.t
    ; letter : char
    ; length : Time_ns.Span.t
    ; categories : Category_id.Set.t
    }
  [@@deriving sexp, bin_io, compare, equal]
end

module Word_status = struct
  type t =
    | Allowed
    | Disallowed
  [@@deriving sexp, compare, equal, bin_io]
end

module Player_and_category = struct
  module T = struct
    type t =
      { player : Player_id.t
      ; category : Category_id.t
      }
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Round_results = struct
  type submission =
    { word : Word.t option
    ; status : Word_status.t
    ; score : int
    }
  [@@deriving sexp, compare, equal, bin_io]

  type t =
    { total_scores : int Player_id.Map.t
    ; by_category : submission Player_id.Map.t Category_id.Map.t
    }
  [@@deriving sexp, compare, equal, bin_io]

  let compute ~player_responses ~categories ~disallowed_words =
    let players = Set.of_map_keys player_responses in
    let by_category =
      List.map categories ~f:(fun category ->
          let words =
            Map.filter_map player_responses ~f:(fun words ->
                Option.join (Map.find words category))
          in
          let players_by_word =
            Map.fold words ~init:Word.Map.empty ~f:(fun ~key:player ~data:word map ->
                if Set.mem disallowed_words { Player_and_category.player; category }
                then map
                else Map.add_multi map ~key:word ~data:player)
          in
          let results =
            Set.to_map players ~f:(fun player ->
                let word = Map.find words player in
                let status =
                  if Set.mem disallowed_words { Player_and_category.player; category }
                  then Word_status.Disallowed
                  else Allowed
                in
                let score =
                  match status with
                  | Disallowed -> 0
                  | Allowed ->
                    (match word with
                    | None -> 0
                    | Some word ->
                      (match Map.find players_by_word word with
                      | None | Some [] -> assert false
                      | Some (_ :: _ :: _) -> 0
                      | Some [ player' ] ->
                        assert (Player_id.( = ) player player');
                        1))
                in
                { word; status; score })
          in
          category, results)
      |> Category_id.Map.of_alist_exn
    in
    let total_scores =
      Map.fold by_category ~init:Player_id.Map.empty ~f:(fun ~key:_ ~data:by_player map ->
          Map.fold by_player ~init:map ~f:(fun ~key:player ~data:submission map ->
              Map.update map player ~f:(fun existing ->
                  let existing = Option.value existing ~default:0 in
                  existing + submission.score)))
    in
    { by_category; total_scores }
  ;;

  (* let%expect_test _ =
   *   let test resps =
   *     let player_responses =
   *       List.map resps ~f:(fun (player_id, responses) ->
   *           ( Player_id.of_string player_id
   *           , List.map responses ~f:(fun (category, word) ->
   *                 Category_id.of_string category, Option.map word ~f:Word.of_string)
   *             |> Category_id.Map.of_alist_exn ))
   *       |> Player_id.Map.of_alist_exn
   *     in
   *     let categories =
   *       Map.fold
   *         player_responses
   *         ~init:Category_id.Set.empty
   *         ~f:(fun ~key:_ ~data:responses categories ->
   *           Set.union categories (Set.of_map_keys responses))
   *       |> Set.to_list
   *     in
   *     let t = compute ~player_responses ~categories in
   *     print_s [%sexp (t : t)]
   *   in
   *   test
   *     [ "player1", [ "Sport", Some "Golf"; "Country", Some "Guyana"; "Colour", None ]
   *     ; ( "player2"
   *       , [ "Sport", Some "Golf"; "Country", Some "Greece"; "Colour", Some "Green" ] )
   *     ]
   * ;; *)
end

module List_zipper = struct
  type 'a focus =
    | Before_start
    | In_list of
        { focus : 'a
        ; prev_rev : 'a list
        }
  [@@deriving sexp, bin_io, compare]

  type 'a t =
    { focus : 'a focus
    ; rest : 'a list
    }
  [@@deriving sexp, bin_io, compare]

  let focused_elt t =
    match t.focus with
    | Before_start -> None
    | In_list { focus; prev_rev = _ } -> Some focus
  ;;

  let create list = { focus = Before_start; rest = list }

  let next t =
    match t.rest with
    | [] -> t
    | new_focus :: rest ->
      let prev_rev =
        match t.focus with
        | Before_start -> []
        | In_list { focus = old_focus; prev_rev } -> old_focus :: prev_rev
      in
      { rest; focus = In_list { focus = new_focus; prev_rev } }
  ;;

  let prev t =
    match t.focus with
    | Before_start -> t
    | In_list { focus = old_focus; prev_rev } ->
      let rest = old_focus :: t.rest in
      let focus =
        match prev_rev with
        | [] -> Before_start
        | new_focus :: rest -> In_list { focus = new_focus; prev_rev = rest }
      in
      { rest; focus }
  ;;
end

module Results_presentation_stage = struct
  type t =
    | Category of Category_id.t
    | Total_scores
  [@@deriving sexp, bin_io, compare]
end

module Game_status = struct
  type t =
    | Not_yet_started of
        { players : Player_id.Set.t
        ; categories_used_so_far : Category_id.Set.t
        }
    | In_play of
        { time_remaining : Time_ns.Stable.Span.V2.t
        ; round_params : Round_params.t
        }
    | Game_finished of
        { scores : Round_results.t
        ; running_totals : int Player_id.Map.t
        ; presentation_stage : Results_presentation_stage.t option
        ; round_params : Round_params.t
        }
  [@@deriving sexp, bin_io, compare]

  let equal = [%compare.equal: t]
end

module Rpc_protocol = struct
  module Log_in = struct
    type query =
      { game_id : Game_id.t
      ; player_id : Player_id.t
      ; password : string
      }
    [@@deriving sexp, bin_io, compare]

    type response' = { you_are_the_owner : bool } [@@deriving sexp, bin_io, compare]
    type response = response' Or_error.t [@@deriving sexp, bin_io, compare]

    let rpc =
      Rpc.Rpc.create
        ~name:"log-in"
        ~version:1
        ~bin_query:[%bin_type_class: query]
        ~bin_response:[%bin_type_class: response]
    ;;
  end

  module Submit_words = struct
    type query =
      { game_id : Game_id.t
      ; player_id : Player_id.t
      ; words : Word.t option Category_id.Map.t
      }
    [@@deriving sexp, bin_io, compare]

    type response = unit Or_error.t [@@deriving sexp, bin_io, compare]

    let rpc =
      Rpc.Rpc.create
        ~name:"submit-words"
        ~version:1
        ~bin_query:[%bin_type_class: query]
        ~bin_response:[%bin_type_class: response]
    ;;
  end

  module Control_game = struct
    type query =
      | Start_round of
          { game_id : Game_id.t
          ; round_params : Round_params.t
          }
      | Next_round of { game_id : Game_id.t }
      | Advance_results_presentation of
          { game_id : Game_id.t
          ; direction : [ `Forward | `Back ]
          }
      | Set_word_status of
          { game_id : Game_id.t
          ; player : Player_id.t
          ; category : Category_id.t
          ; status : Word_status.t
          }
    [@@deriving sexp, bin_io, compare]

    type response = unit Or_error.t [@@deriving sexp, bin_io, compare]

    let rpc =
      Rpc.Rpc.create
        ~name:"control-game"
        ~version:1
        ~bin_query:[%bin_type_class: query]
        ~bin_response:[%bin_type_class: response]
    ;;
  end

  module Get_game_status = struct
    type query =
      { game_id : Game_id.t
      ; player_id : Player_id.t
      }
    [@@deriving sexp, bin_io, compare]

    type response = Game_status.t Or_error.t [@@deriving sexp, bin_io, compare]

    let rpc =
      Rpc.Rpc.create
        ~name:"get-game-status"
        ~version:1
        ~bin_query:[%bin_type_class: query]
        ~bin_response:[%bin_type_class: response]
    ;;
  end
end
