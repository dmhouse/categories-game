open! Core_kernel
open Import

(** The main game page, where players submit their words. *)

val bonsai
  :  round_params:Round_params.t Bonsai.Value.t
  -> clock:Clock.t
  -> submit_words:(Word.t option Category_id.Map.t -> unit Bonsai.Effect.t) Bonsai.Value.t
  -> time_remaining:Time_ns.Span.t Bonsai.Value.t
  -> Vdom.Node.t Bonsai.Computation.t
