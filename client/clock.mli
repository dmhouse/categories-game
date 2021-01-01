open! Core_kernel
open Import

(** A module used to schedule various timer events in categories. *)

(* TODO-someday: an expanded version of this should just be part of bonsai itself *)

type t

val create : now:Time_ns.t -> t
val advance : t -> now:Time_ns.t -> unit

(** Arrange for [f] to be called every [span]. *)
val every
  :  t
  -> Time_ns.Span.t
  -> f:(unit -> Ui_event.t) Bonsai.Value.t
  -> unit Bonsai.Computation.t
