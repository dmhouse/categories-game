open Core
open Async
open Scattegories_common

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

module Files = struct
  type t =
    { js : Filename.t
    ; css : Filename.t
    ; html : Filename.t
    }

  let param =
    let%map_open.Command js =
      flag "js" (required Filename.arg_type) ~doc:" path to javascript"
    and css = flag "css" (required Filename.arg_type) ~doc:" path to css"
    and html = flag "html" (required Filename.arg_type) ~doc:" path to html" in
    { js; css; html }
  ;;
end

let static_files_handler ~body:_ _sock (request : Cohttp.Request.t) ~(files : Files.t) =
  match request.resource with
  | "/scattegories.bc.js" -> Cohttp_async.Server.respond_with_file files.js
  | "/style.css" -> Cohttp_async.Server.respond_with_file files.css
  | "/" | "/index.html" -> Cohttp_async.Server.respond_with_file files.html
  | _ ->
    printf "404: %s\n" request.resource;
    Cohttp_async.Server.respond_string "Not found" ~status:`Not_found
;;

let categories _rstate =
  List.map ~f:Category_id.of_string [ "Sport"; "Country"; "Girl's name"; "Boy's name" ]
  |> Category_id.Set.of_list
;;

module Game_state = struct
  module Finished_status = struct
    type t =
      { scores : Game_results.t
      ; presentation_stages : Results_presentation_stage.t List_zipper.t
      ; words : Word.t option Category_id.Map.t Player_id.Map.t
      ; game_info : Game_info.t
      ; disallowed_words : Player_and_category.Set.t
      }
    [@@deriving sexp_of]
  end

  type status =
    | Not_yet_started
    | Started of
        { end_at : Time_ns.t
        ; words : Word.t option Category_id.Map.t Player_id.Map.t
        ; game_info : Game_info.t
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
  [@@deriving sexp_of]

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

  let start_round t ~end_at ~game_info =
    t.status <- Started { game_info; end_at; words = Player_id.Map.empty }
  ;;

  let end_round_and_compute_results t =
    match t.status with
    | Not_yet_started | Finished _ -> ()
    | Started { words; game_info; _ } ->
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
                        (Char.to_string game_info.letter)
                  in
                  if is_allowed then set else Set.add set { player; category }))
      in
      let scores =
        Game_results.compute
          ~player_responses:words
          ~categories:(Set.to_list game_info.categories)
          ~disallowed_words
      in
      let presentation_stages =
        List.map (Category_id.Set.to_list game_info.categories) ~f:(fun category ->
            Results_presentation_stage.Category category)
        @ [ Total_scores ]
      in
      t.status
        <- Finished
             { scores
             ; presentation_stages = List_zipper.create presentation_stages
             ; words
             ; game_info
             ; disallowed_words
             }
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
    | Not_yet_started | Started _ -> ()
    | Finished finished ->
      t.running_totals_ex_this_round <- running_totals t finished;
      t.categories_used_so_far
        <- Set.union t.categories_used_so_far finished.game_info.categories;
      t.status <- Not_yet_started
  ;;
end

module State = struct
  type t =
    { games : Game_state.t Game_id.Table.t
    ; rstate : Random.State.t
    ; time_source : Time_source.t
    }

  let accept_login t ~game_id ~player_id =
    let game_state =
      Hashtbl.find_or_add t.games game_id ~default:(fun () ->
          let game_state = Game_state.create t.rstate ~id:game_id ~owner:player_id in
          print_s
            [%message
              "Created new game" (game_id : Game_id.t) (game_state : Game_state.t)];
          game_state)
    in
    print_s
      [%message "Adding player to game" (game_id : Game_id.t) (player_id : Player_id.t)];
    Game_state.add_player game_state player_id;
    game_state
  ;;

  let create rstate time_source = { games = Game_id.Table.create (); rstate; time_source }
end

let log_in
    (state : State.t)
    ()
    ({ Rpc_protocol.Log_in.player_id; game_id; password } as query)
  =
  print_s [%message "login attempt" (query : Rpc_protocol.Log_in.query)];
  if String.( <> ) password "HoHoHo"
  then Or_error.error_string "wrong password"
  else (
    let game_state = State.accept_login state ~game_id ~player_id in
    let you_are_the_owner = Player_id.( = ) game_state.owner player_id in
    Ok { Rpc_protocol.Log_in.you_are_the_owner })
;;

let start_round (state : State.t) () { Rpc_protocol.Start_round.game_id; game_info } =
  print_s [%message "Starting game" (game_id : Game_id.t)];
  Option.iter (Hashtbl.find state.games game_id) ~f:(fun game_state ->
      let end_at = Time_ns.add (Time_source.now state.time_source) game_info.length in
      Game_state.start_round game_state ~end_at ~game_info;
      upon (Time_source.at state.time_source end_at) (fun () ->
          Game_state.end_round_and_compute_results game_state;
          print_s
            [%message
              "Game finished" (game_id : Game_id.t) ~_:(game_state : Game_state.t)]))
;;

let get_game_status
    (state : State.t)
    ()
    { Rpc_protocol.Get_game_status.game_id; player_id = _ }
  =
  match Hashtbl.find state.games game_id with
  | None -> error_s [%message "Unknown game" (game_id : Game_id.t)]
  | Some game_state ->
    (match game_state.status with
    | Not_yet_started ->
      Ok
        (Game_status.Not_yet_started
           { players = Game_state.players game_state
           ; categories_used_so_far = game_state.categories_used_so_far
           })
    | Started { end_at; game_info; words = _ } ->
      let time_remaining =
        Time_ns.diff end_at (Time_ns.now ()) |> Time_ns.Span.max Time_ns.Span.zero
      in
      Ok (In_play { time_remaining; game_info })
    | Finished finished ->
      Ok
        (Game_finished
           { scores = finished.scores
           ; presentation_stage = List_zipper.focused_elt finished.presentation_stages
           ; running_totals = Game_state.running_totals game_state finished
           ; game_info = finished.game_info
           }))
;;

let submit_words
    (state : State.t)
    ()
    { Rpc_protocol.Submit_words.game_id; player_id; words }
  =
  match Hashtbl.find state.games game_id with
  | None -> error_s [%message "Unknown game" (game_id : Game_id.t)]
  | Some game_state ->
    (match game_state.status with
    | Not_yet_started | Finished _ -> error_s [%message "Game not in play"]
    | Started { words = words_by_player; end_at; game_info } ->
      print_s
        [%message
          "Setting words"
            (game_id : Game_id.t)
            (player_id : Player_id.t)
            (words : Word.t option Category_id.Map.t)];
      game_state.status
        <- Started
             { end_at
             ; game_info
             ; words = Map.set words_by_player ~key:player_id ~data:words
             };
      Ok ())
;;

let advance_results_presentation
    (state : State.t)
    ()
    { Rpc_protocol.Advance_results_presentation.game_id; direction }
  =
  match Hashtbl.find state.games game_id with
  | None -> error_s [%message "Unknown game" (game_id : Game_id.t)]
  | Some game_state ->
    (match game_state.status with
    | Not_yet_started | Started _ -> error_s [%message "Game not finished yet"]
    | Finished finished ->
      let presentation_stages =
        match direction with
        | `Forward -> List_zipper.next finished.presentation_stages
        | `Back -> List_zipper.prev finished.presentation_stages
      in
      game_state.status <- Finished { finished with presentation_stages };
      Ok ())
;;

let set_word_status
    (state : State.t)
    ()
    { Rpc_protocol.Set_word_status.game_id; player; category; status }
  =
  match Hashtbl.find state.games game_id with
  | None -> error_s [%message "Unknown game" (game_id : Game_id.t)]
  | Some game_state ->
    (match game_state.status with
    | Not_yet_started | Started _ -> error_s [%message "Game not finished yet"]
    | Finished finished ->
      print_s
        [%message
          "Setting word status"
            (game_id : Game_id.t)
            (player : Player_id.t)
            (category : Category_id.t)
            (status : Word_status.t)];
      let player_and_category = { Player_and_category.player; category } in
      let disallowed_words =
        match status with
        | Allowed -> Set.remove finished.disallowed_words player_and_category
        | Disallowed -> Set.add finished.disallowed_words player_and_category
      in
      let scores =
        Game_results.compute
          ~player_responses:finished.words
          ~categories:(Set.to_list finished.game_info.categories)
          ~disallowed_words
      in
      game_state.status <- Finished { finished with scores; disallowed_words };
      Ok ())
;;

let next_round (state : State.t) () { Rpc_protocol.Next_round.game_id } =
  match Hashtbl.find state.games game_id with
  | None -> error_s [%message "Unknown game" (game_id : Game_id.t)]
  | Some game_state ->
    (match game_state.status with
    | Not_yet_started | Started _ -> error_s [%message "Game not finished yet"]
    | Finished _ ->
      Game_state.next_round game_state;
      Ok ())
;;

let implementations state =
  Rpc.Implementations.create_exn
    ~implementations:
      [ Rpc.Rpc.implement' Rpc_protocol.Log_in.rpc (log_in state)
      ; Rpc.Rpc.implement' Rpc_protocol.Start_round.rpc (start_round state)
      ; Rpc.Rpc.implement' Rpc_protocol.Submit_words.rpc (submit_words state)
      ; Rpc.Rpc.implement'
          Rpc_protocol.Advance_results_presentation.rpc
          (advance_results_presentation state)
      ; Rpc.Rpc.implement' Rpc_protocol.Next_round.rpc (next_round state)
      ; Rpc.Rpc.implement' Rpc_protocol.Set_word_status.rpc (set_word_status state)
      ; Rpc.Rpc.implement' Rpc_protocol.Get_game_status.rpc (get_game_status state)
      ]
    ~on_unknown_rpc:`Raise
;;

let go ~files ~rstate ~time_source =
  let state = State.create rstate time_source in
  let handler =
    Cohttp_async_websocket.Server.create
      ~opcode:`Binary
      ~non_ws_request:(static_files_handler ~files)
      (fun ~inet:_ ~subprotocol:_ _request ->
        Cohttp_async_websocket.Server.On_connection.create (fun reader writer ->
            let open Async_rpc_kernel in
            let transport = Pipe_transport.(create Kind.string reader writer) in
            let%bind conn =
              Rpc.Connection.create
                transport
                ~connection_state:(Fn.const ())
                ~implementations:(implementations state)
              >>| Result.ok_exn
            in
            let%bind () = Rpc.Connection.close_finished conn in
            Rpc.Transport.close transport)
        |> Deferred.return)
  in
  let%bind _server =
    Cohttp_async.Server.create_expert
      ~on_handler_error:`Ignore
      (Tcp.Where_to_listen.of_port 8000)
      handler
  in
  Deferred.never ()
;;

let () =
  Command.async
    ~summary:"start server"
    (let%map_open.Command files = Files.param in
     fun () ->
       go
         ~files
         ~rstate:(Random.State.make_self_init ())
         ~time_source:(Time_source.wall_clock ()))
  |> Command.run
;;
