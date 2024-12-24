open Interpolation

type interpolation_type = Linear | Lagrange | All

let print_step interpolation_fun input step =
  input
  |> List.fold_left (fun acc str -> parse_point str acc) []
  |> is_sorted
  |> fun points -> interpolation_fun points step |> print_interpolation_result

let collect_points prompt n =
  Printf.printf "%s\n" prompt;
  let rec collect acc =
    match List.length acc with
    | l when l = n -> acc
    | _ -> (
        match read_line () with "exit" -> exit 0 | p -> collect (p :: acc))
  in
  collect []

let linear points step =
  Printf.printf "Linear interpolation:\n";
  print_step linear_interpolation
    [ List.hd points; List.hd (List.tl points) ]
    step

let lagrange points step =
  cut_window points
  |> List.rev
  |> fun input ->
  Printf.printf "Lagrange interpolation:\n";
  print_step lagrange_interpolation input step

let interpolate interpolation_fun step =
  let points =
    match interpolation_fun with
    | Lagrange -> collect_points "Enter 4 points for Interpolation:" 4
    | Linear -> collect_points "Enter 2 points for Linear Interpolation:" 2
    | All -> collect_points "Enter 2 points for Interpolation:" 2
  in
  if Lagrange = interpolation_fun then
    lagrange points step
  else
    linear points step;

  let rec collect_next_point points =
    Printf.printf "Enter next point: \n";
    match read_line () with
    | "exit" -> exit 0
    | p -> (
        let n_points = p :: points in
        Printf.printf "Length of points: %d\n" (List.length n_points);
        match List.length n_points with
        | n when n >= 4 -> (
            match interpolation_fun with
            | Lagrange ->
                lagrange n_points step;
                collect_next_point n_points
            | All ->
                linear n_points step;
                lagrange n_points step;
                collect_next_point n_points
            | _ ->
                linear n_points step;
                collect_next_point n_points)
        | _ ->
            linear n_points step;
            collect_next_point n_points)
  in
  collect_next_point points

let rec enter_step () =
  Printf.printf "Enter interpolation step (float number): \n";
  try read_line () |> float_of_string with _ -> enter_step ()

let rec intro () =
  Printf.printf "Choose interpolation method (linear, lagrange, all): \n";
  let interpolation = read_line () in
  match interpolation with
  | "lagrange" -> interpolate Lagrange (enter_step ())
  | "linear" -> interpolate Linear (enter_step ())
  | "all" -> interpolate All (enter_step ())
  | _ ->
      Printf.printf "Invalid interpolation method. Try again \n";
      intro ()

let () = intro ()
