open OUnit2
open Interpolation

let epsilon = 0.01

let points =
  [
    { x = 0.; y = 0. };
    { x = 1.57; y = 1. };
    { x = 3.142; y = 0. };
    { x = 4.712; y = -1. };
  ]

let points_unsorted =
  [
    { x = 0.; y = 0. };
    { x = 3.142; y = 0. };
    { x = 1.57; y = 1. };
    { x = 4.712; y = -1. };
  ]

let points_long =
  [
    { x = 0.; y = 0. };
    { x = 1.57; y = 1. };
    { x = 3.142; y = 0. };
    { x = 4.712; y = -1. };
    { x = 12.568; y = 0. };
  ]

let two_points = [ { x = 0.; y = 0. }; { x = 1.57; y = 1. } ]

let test_is_sorted _ =
  assert_equal (is_sorted points) points;
  assert_raises (Failure "Points are not sorted") (fun () ->
      is_sorted points_unsorted)

let test_linear_interpolation _ =
  let expected = [ { x = 1.; y = 0.64 }; { x = 0.; y = 0. } ] in
  assert_equal
    ~cmp:
      (List.for_all2 (fun p1 p2 ->
           abs_float (p1.x -. p2.x) < epsilon
           && abs_float (p1.y -. p2.y) < epsilon))
    expected
    (linear_interpolation two_points 1.)

let test_cut_window _ =
  assert_equal
    [
      { x = 1.57; y = 1. };
      { x = 3.142; y = 0. };
      { x = 4.712; y = -1. };
      { x = 12.568; y = 0. };
    ]
    (cut_window points_long);
  assert_equal points (cut_window points)

let test_lagrange_interpolation _ =
  let expected =
    [
      { x = 0.; y = 0. };
      { x = 2.; y = 20. };
      { x = 4.; y = 40. };
      { x = 6.; y = 60. };
      { x = 8.; y = 80. };
      { x = 10.; y = 100. };
    ]
  in
  assert_equal
    ~cmp:
      (List.for_all2 (fun p1 p2 ->
           abs_float (p1.x -. p2.x) < epsilon
           && abs_float (p1.y -. p2.y) < epsilon))
    expected
    (List.rev
       (lagrange_interpolation
          [
            { x = 0.; y = 0. };
            { x = 1.; y = 10. };
            { x = 5.; y = 50. };
            { x = 10.; y = 100. };
          ]
          2.))

let suite =
  "suite"
  >::: [
         "test_is_sorted" >:: test_is_sorted;
         "test_linear_interpolation" >:: test_linear_interpolation;
         "test_cut_window" >:: test_cut_window;
         "test_lagrange_interpolation" >:: test_lagrange_interpolation;
       ]

let () = run_test_tt_main suite
