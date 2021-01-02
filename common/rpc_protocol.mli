open Core_kernel
open Types
open Async_js

module Log_in : sig
  (** Used by players to join a game. *)

  type query =
    { game_id : Game_id.t
    ; player : Player_id.t
    ; password : string
    }
  [@@deriving sexp, bin_io, compare]

  type response' = { you_are_the_owner : bool } [@@deriving sexp, bin_io, compare]
  type response = response' Or_error.t [@@deriving sexp, bin_io, compare]

  val rpc : (query, response) Rpc.Rpc.t
end

module Submit_words : sig
  (** During a round, all clients regularly send their results to the server
     using this RPC. *)

  type query = Word.t option Category_id.Map.t [@@deriving sexp, bin_io, compare]
  type response = unit Or_error.t [@@deriving sexp, bin_io, compare]

  val rpc : (query, response) Rpc.Rpc.t
end

module Control_game : sig
  (** The owner can use this RPC to change the state of the game. *)

  type query =
    | Start_round of { round_params : Round_params.t }
        (** Once the game details have been chosen on the pre-round page, this
       starts the round. *)
    | Next_round
        (** Once the results have been fully presented, this moves the game state
       back to the pre-round page. *)
    | Advance_results_presentation of [ `Forward | `Back ]
    | Set_word_status of
        { player : Player_id.t
        ; category : Category_id.t
        ; status : Word_status.t
        }
        (** Used during the results presentation to either allow or disallow a
             given submission. *)
  [@@deriving sexp, bin_io, compare]

  type response = unit Or_error.t [@@deriving sexp, bin_io, compare]

  val rpc : (query, response) Rpc.Rpc.t
end

module Get_game_status : sig
  (** Clients call this RPC to continually get the latest status from the
     server. *)

  type query = unit [@@deriving sexp, bin_io, compare]
  type response = Game_status.t Or_error.t [@@deriving sexp, bin_io, compare]

  val rpc : (query, response) Rpc.Rpc.t
end

module Which : sig
  type ('query, 'response) t =
    | Log_in : (Log_in.query, Log_in.response) t
    | Submit_words : (Submit_words.query, Submit_words.response) t
    | Control_game : (Control_game.query, Control_game.response) t
    | Get_game_status : (Get_game_status.query, Get_game_status.response) t
  [@@deriving sexp_of]

  type packed = Pack : ('q, 'r) t -> packed [@@deriving sexp_of]

  val all : packed list
  val rpc : ('q, 'r) t -> ('q, 'r) Rpc.Rpc.t
end
