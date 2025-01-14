open Printf
open Interpolation

type interpolation_type = {
  name : string;
  required_points : int;
  interpolate : point list -> float -> point list;
}

let interpolation_methods =
  [
    { name = "linear"; required_points = 2; interpolate = linear_interpolation };
    {
      name = "lagrange";
      required_points = 4;
      interpolate = lagrange_interpolation;
    };
  ]

let parse_point line =
  match
    String.split_on_char ' ' (String.trim line) |> List.map float_of_string_opt
  with
  | [ Some x; Some y ] -> Some { x; y }
  | _ -> None

let print_step interpolation input step =
  Printf.printf "%s interpolation:\n" interpolation.name;
  cut_window input
  |> is_sorted
  |> fun points ->
  interpolation.interpolate points step |> print_interpolation_result

let input_points n =
  let rec loop acc =
    match List.length acc with
    | a when a = n -> acc
    | _ -> (
        match read_line () with
        | "exit" -> exit 0
        | line -> (
            match parse_point line with
            | Some point -> loop (point :: acc)
            | None ->
                Printf.printf "Invalid point format. Try again (x y):\n";
                loop acc))
  in
  loop [] |> List.rev

let interpolate interpolation step =
  Printf.printf "Enter %d points for Interpolation:\n"
    interpolation.required_points;
  let points = input_points interpolation.required_points in
  print_step interpolation points step;

  let rec collect_next_point points =
    Printf.printf "Enter next point: \n";
    let p = input_points 1 in
    let n_points = points @ p in
    print_step interpolation n_points step;
    collect_next_point n_points
  in
  collect_next_point points

let rec interpolation_choice () =
  Printf.printf "Choose interpolation method (linear, lagrange, all):\n";
  match read_line () with
  | "lagrange" -> List.nth interpolation_methods 1
  | "linear" -> List.nth interpolation_methods 0
  | "all" -> List.nth interpolation_methods 0
  | _ ->
      Printf.printf "Invalid choice. Try again.\n";
      interpolation_choice ()

let rec enter_step () =
  printf "Enter interpolation step (float number):\n";
  match read_line () |> float_of_string_opt with
  | Some step when step > 0.0 -> step
  | _ ->
      printf "Invalid step. Try again.\n";
      enter_step ()

let main () =
  let interpolation = interpolation_choice () in
  let step = enter_step () in
  interpolate interpolation step

let () = main ()
