open Base

type 'a t =
  | No_focus of
      { forward : 'a list
      ; backward : 'a list
      }
  | Focus of
      { prev_rev : 'a list
      ; focus : 'a
      ; rest : 'a list
      }
[@@deriving sexp_of, compare]

let focus = function
  | No_focus _ -> None
  | Focus { focus; _ } -> Some focus
;;

let create list = No_focus { forward = list; backward = List.rev list }

let next = function
  | No_focus { forward; backward = _ } as t ->
    (match forward with
    | [] -> t
    | hd :: tl -> Focus { prev_rev = []; focus = hd; rest = tl })
  | Focus { prev_rev; focus; rest } ->
    let prev_rev = focus :: prev_rev in
    (match rest with
    | [] -> No_focus { forward = List.rev prev_rev; backward = prev_rev }
    | hd :: tl -> Focus { prev_rev; focus = hd; rest = tl })
;;

let prev = function
  | No_focus { forward = _; backward } as t ->
    (match backward with
    | [] -> t
    | hd :: tl -> Focus { prev_rev = tl; focus = hd; rest = [] })
  | Focus { prev_rev; focus; rest } ->
    let rest = focus :: rest in
    (match prev_rev with
    | [] -> No_focus { forward = rest; backward = List.rev rest }
    | hd :: tl -> Focus { prev_rev = tl; focus = hd; rest })
;;

let rev = function
  | No_focus { forward; backward } -> No_focus { forward = backward; backward = forward }
  | Focus { prev_rev; focus; rest } -> Focus { prev_rev = rest; focus; rest = prev_rev }
;;
