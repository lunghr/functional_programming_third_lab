open Interpolation

let parse_point line =
  match
    String.split_on_char ' ' (String.trim line) |> List.map float_of_string_opt
  with
  | [ Some x; Some y ] -> Some { x; y }
  | _ -> None

let print_step interpolations input step =
  Printf.printf "%s interpolation:\n" interpolations.name;
  List.iter
    (fun interpolation ->
      cut_window input
      |> is_sorted
      |> fun points -> interpolation points step |> print_interpolation_result)
    interpolations.interpolate

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

let interpolation_choice method_name =
  match method_name with
  | "lagrange" -> List.nth interpolation_methods 1
  | "linear" -> List.nth interpolation_methods 0
  | "all" -> List.nth interpolation_methods 2
  | _ ->
      failwith "Invalid interpolation method. Choose: linear, lagrange, or all."

let get_step step_str =
  match float_of_string_opt step_str with
  | Some step when step > 0.0 -> step
  | _ -> failwith "Invalid step. Step must be a positive float number."

let main () =
  let args = Array.to_list Sys.argv |> List.tl in
  match args with
  | [ method_name; step_str ] ->
      let interpolation = interpolation_choice method_name in
      let step = get_step step_str in
      interpolate interpolation step
  | _ ->
      failwith
        "Usage: dune exec _build/default/bin/main.exe -- \
         <interpolation_method> <step>"

let () = main ()
