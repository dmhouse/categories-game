open Core_kernel
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
    { letter : char
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

module Results_presentation_stage = struct
  type t =
    | Category of Category_id.t
    | Total_scores
  [@@deriving sexp, bin_io, compare]
end
