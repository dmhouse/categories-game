open Base
open List_zipper

let print t =
  Stdio.print_endline
    (match focus t with
    | None -> "-"
    | Some i -> Int.to_string i)
;;

let%expect_test "basic movement" =
  let t = create [ 1; 2; 3; 4; 5 ] in
  print t;
  [%expect {| - |}];
  let t = next t in
  print t;
  [%expect {| 1 |}];
  let t = next t in
  print t;
  [%expect {| 2 |}];
  let t = next (next t) in
  print t;
  [%expect {| 4 |}];
  let t = prev t in
  print t;
  [%expect {| 3 |}]
;;

let%expect_test "navigating from no focus" =
  let t = create [ 10; 11 ] in
  print t;
  [%expect {| - |}];
  print (prev t);
  [%expect {| 11 |}];
  print (next t);
  [%expect {| 10 |}]
;;

let%expect_test "navigating off the start" =
  let t = create [ 10; 11 ] in
  print t;
  [%expect {| - |}];
  let t = next t in
  print t;
  [%expect {| 10 |}];
  let t = prev t in
  print t;
  [%expect {| - |}]
;;

let%expect_test "navigating off the end" =
  let t = create [ 10; 11 ] in
  print t;
  [%expect {| - |}];
  let t = next (next t) in
  print t;
  [%expect {| 11 |}];
  let t = next t in
  print t;
  [%expect {| - |}]
;;

let%expect_test "rev" =
  let t = create [ 20; 30; 40; 50 ] in
  let t = next (next (next t)) in
  print t;
  [%expect {| 40 |}];
  let t = rev t in
  print t;
  [%expect {| 40 |}];
  let t = next t in
  print t;
  [%expect {| 30 |}];
  let t = next t in
  print t;
  [%expect {| 20 |}];
  let t = next t in
  print t;
  [%expect {| - |}]
;;

module Quickcheck = struct
  type nonrec t = int t [@@deriving sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator in
    let open Let_syntax in
    let no_focus = list int >>| List_zipper.create in
    let with_focus =
      let%map prev_rev = list int
      and focus = int
      and rest = list int in
      List_zipper.For_testing.create_with_focus ~prev_rev ~focus ~rest
    in
    weighted_union [ 0.3, no_focus; 0.7, with_focus ]

  let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
end

let%test_unit "quickcheck" =
  Base_quickcheck.Test.run_exn
    (module Quickcheck)
    ~f:(fun t ->
      [%test_eq: int option] (focus (next t)) (focus (prev (rev t)));
      [%test_eq: int option] (focus (prev t)) (focus (next (rev t)));
      [%test_eq: int option] (focus t) (focus (rev t));
      [%test_eq: int option] (focus (prev (next t))) (focus t);
      [%test_eq: int option] (focus (next (prev t))) (focus t))
;;
