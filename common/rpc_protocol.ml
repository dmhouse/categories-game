open Core_kernel
open Types
open Async_js

module Log_in = struct
  type query =
    { game_id : Game_id.t
    ; player : Player_id.t
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
  type query = Word.t option Category_id.Map.t [@@deriving sexp, bin_io, compare]
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
    | Start_round of { round_params : Round_params.t }
    | Next_round
    | Advance_results_presentation of [ `Forward | `Back ]
    | Set_word_status of
        { player : Player_id.t
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
  type query = unit [@@deriving sexp, bin_io, compare]
  type response = Game_status.t Or_error.t [@@deriving sexp, bin_io, compare]

  let rpc =
    Rpc.Rpc.create
      ~name:"get-game-status"
      ~version:1
      ~bin_query:[%bin_type_class: query]
      ~bin_response:[%bin_type_class: response]
  ;;
end

module Which = struct
  module T = struct
    type ('query, 'response) t =
      | Log_in : (Log_in.query, Log_in.response) t
      | Submit_words : (Submit_words.query, Submit_words.response) t
      | Control_game : (Control_game.query, Control_game.response) t
      | Get_game_status : (Get_game_status.query, Get_game_status.response) t
    [@@deriving sexp_of]

    let rpc (type q r) (t : (q, r) t) : (q, r) Rpc.Rpc.t =
      match t with
      | Log_in -> Log_in.rpc
      | Submit_words -> Submit_words.rpc
      | Control_game -> Control_game.rpc
      | Get_game_status -> Get_game_status.rpc
    ;;
  end

  include T
  include Bonsai_rpc_wrapper.Make (T)

  let all = [ Pack Log_in; Pack Submit_words; Pack Control_game; Pack Get_game_status ]
end
