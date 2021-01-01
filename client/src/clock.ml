open! Core_kernel
open Import

type t = Bonsai_web.Incr.Clock.t

let create ~now = Bonsai_web.Incr.Clock.create ~start:now ()
let advance t ~now = Bonsai_web.Incr.Clock.advance_clock t ~to_:now

module Time_ns_with_sexp_and_equal = struct
  type t = Time_ns.Alternate_sexp.t [@@deriving sexp]

  let equal = [%compare.equal: Time_ns.t]
end

let every t span ~f =
  let value =
    (* Making the value be a Time_ns rather than a Unit is necessary, because
       inside [Bonsai.Edge.on_change], it stores our value in a [Bonsai.state],
       which always uses phys_equal as the cutoff.
     *)
    Bonsai_web.Incr.Clock.at_intervals t span
    |> Bonsai_web.Incr.map ~f:(fun () -> Bonsai_web.Incr.Clock.now t)
    |> Bonsai.Incr.to_value
  in
  let callback =
    let%map f = f in fun _time -> f () in
  Bonsai.Edge.on_change [%here] (module Time_ns_with_sexp_and_equal) value ~callback
;;
