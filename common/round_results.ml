open Core_kernel
open Types

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
