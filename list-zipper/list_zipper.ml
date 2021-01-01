open Base

type 'a focus =
  | Before_start
  | In_list of
      { focus : 'a
      ; prev_rev : 'a list
      }
[@@deriving sexp_of, compare]

type 'a t =
  { focus : 'a focus
  ; rest : 'a list
  }
[@@deriving sexp_of, compare]

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
