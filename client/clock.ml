open! Core_kernel
open Import

type t = Bonsai_web.Incr.Clock.t

let create ~now = Bonsai_web.Incr.Clock.create ~start:now ()
let advance t ~now = Bonsai_web.Incr.Clock.advance_clock t ~to_:now

let every t span ~f =
  let%sub value =
    Bonsai_web.Incr.Clock.at_intervals t span
    |> Bonsai.Incr.to_value
    (* Physical equality is currently the default, but it's essential for this
       module to work, so let's make extra sure. *)
    |> Bonsai.Incr.value_cutoff ~equal:phys_equal
  in
  Bonsai.Edge.on_change [%here] (module Unit) value ~callback:f
;;
