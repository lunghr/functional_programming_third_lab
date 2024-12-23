open Interpolation

let lagrange_print input =
  input
  |> List.fold_left (fun acc str -> parse_point str acc) []
  |> sort_points
  |> fun points ->
  Printf.printf "Lagrange Interpolation: \n";
  lagrange_interpolation points 1. |> print_interpolation_result

let lagrange () =
  Printf.printf "Enter 4 points for Lagrange Interpolation: \n";
  let rec collect_first_four_points acc =
    match List.length acc with
    | 4 -> acc
    | _ -> (
        match read_line () with
        | "exit" -> exit 0
        | p -> collect_first_four_points (p :: acc))
  in
  collect_first_four_points []
  |> fun points ->
  lagrange_print points;
  let rec collect_next_point points =
    Printf.printf "Enter next point for Lagrange Interpolation: \n";
    match read_line () with
    | "exit" -> exit 0
    | p ->
        cut_window (p :: points)
        |> lagrange_print
        |> fun _ -> collect_next_point (p :: points)
  in
  collect_next_point points

let linear_print input =
  input
  |> List.fold_left (fun acc str -> parse_point str acc) []
  |> sort_points
  |> fun points ->
  Printf.printf "Linear Interpolation: \n";
  linear_interpolation points 1. |> print_interpolation_result

let rec linear = function
  | [] -> (
      Printf.printf "Enter 2 points for Linear Interpolation: \n";
      match read_line () with
      | "exit" -> exit 0
      | p1 ->
          read_line ()
          |> fun p2 ->
          linear_print [ p1; p2 ];
          linear [ p2 ])
  | p1 :: _ -> (
      Printf.printf "Enter next point for Linear Interpolation: \n";
      match read_line () with
      | "exit" -> exit 0
      | p2 ->
          linear_print [ p1; p2 ];
          linear [ p2 ])

let all () =
  Printf.printf "Enter 2 points: \n";
  let rec collect_first_two_points acc =
    match List.length acc with
    | 2 -> acc
    | _ -> (
        match read_line () with
        | "exit" -> exit 0
        | p -> collect_first_two_points (p :: acc))
  in
  collect_first_two_points []
  |> fun points ->
  linear_print points;
  lagrange_print points;
  let rec collect_next_point points =
    Printf.printf "Enter next point: \n";
    match read_line () with
    | "exit" -> exit 0
    | p -> (
        match List.length (p :: points) with
        | 4 ->
            linear_print [ List.hd points; p ];
            cut_window (p :: points) |> lagrange_print;
            collect_next_point (p :: points)
        | _ ->
            linear_print [ List.hd points; p ];
            collect_next_point (p :: points))
  in

  collect_next_point points

let rec intro () =
  Printf.printf "Choose interpolation method (linear, lagrange, all): \n";
  match read_line () with
  | "lagrange" -> lagrange ()
  | "linear" -> linear []
  | "all" -> all ()
  | _ -> intro ()

let () = intro ()
