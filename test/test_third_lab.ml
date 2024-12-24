open OUnit2
open Interpolation

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

let test_parse_point _ =
  assert_equal [ { x = 0.; y = 0. } ] (parse_point "0. 0." []);
  assert_equal [ { x = 1.57; y = 1. } ] (parse_point "1.57 1." []);
  assert_raises (Failure "Invalid data format for point") (fun () ->
      parse_point "0.9" []);
  assert_raises (Failure "Invalid data format for point") (fun () ->
      parse_point "0. 0. 0." []);
  assert_raises (Failure "Invalid data format for point") (fun () ->
      parse_point "error" [])

let test_linear_interpolation _ =
  assert_equal
    [
      { x = 2.; y = 1.27388535031847128 };
      { x = 1.; y = 0.636942675159235638 };
      { x = 0.; y = 0. };
    ]
    (linear_interpolation two_points 1.);
  assert_raises (Failure "Invalid data format for interpolation") (fun () ->
      linear_interpolation points 1.);
  assert_raises (Failure "Invalid data format for interpolation") (fun () ->
      linear_interpolation points_unsorted 1.)

let test_lagrange_polynomial _ =
  assert_equal 0. (lagrange_polynomial points 0.);
  assert_equal 1. (lagrange_polynomial points 1.57);
  assert_equal 0. (lagrange_polynomial points 3.142);
  assert_equal (-1.) (lagrange_polynomial points 4.712)

let test_cut_window _ =
  assert_equal
    [
      { x = 1.57; y = 1. };
      { x = 3.142; y = 0. };
      { x = 4.712; y = -1. };
      { x = 12.568; y = 0. };
    ]
    (cut_window (List.rev points_long));
  assert_equal points (cut_window (List.rev points))

let test_lagrange_interpolation _ =
  assert_equal
    [
      { x = 5.; y = -1.02585330905177052 };
      { x = 4.; y = -0.673906611578378612 };
      { x = 3.; y = 0.120257212496086133 };
      { x = 2.; y = 0.841033018739032 };
      { x = 1.; y = 0.972815662717867236 };
      { x = 0.; y = 0. };
    ]
    (lagrange_interpolation points 1.);
  assert_equal
    [
      { x = 10.; y = 100. };
      { x = 9.; y = 90. };
      { x = 8.; y = 80. };
      { x = 7.; y = 70. };
      { x = 6.; y = 59.9999999999999929 };
      { x = 5.; y = 50. };
      { x = 4.; y = 40. };
      { x = 3.; y = 30.0000000000000036 };
      { x = 2.; y = 20. };
      { x = 1.; y = 10. };
      { x = 0.; y = 0. };
    ]
    (lagrange_interpolation
       [ { x = 0.; y = 0. }; { x = 1.; y = 10. }; { x = 10.; y = 100. } ]
       1.)

let suite =
  "suite"
  >::: [
         "test_is_sorted" >:: test_is_sorted;
         "test_parse_point" >:: test_parse_point;
         "test_linear_interpolation" >:: test_linear_interpolation;
         "test_lagrange_polynomial" >:: test_lagrange_polynomial;
         "test_cut_window" >:: test_cut_window;
            "test_lagrange_interpolation" >:: test_lagrange_interpolation
       ]

let () = run_test_tt_main suite
