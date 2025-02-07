type point = { x : float; y : float }

let print_interpolation_result points =
  if List.length points < 2 then
    match List.length points with
    | len when len > 0 ->
        Printf.printf
          "Step is too big for this interpolation diapason, enter more points, \
           please\n"
    | _ -> ()
  else
    points
    |> List.fold_left
         (fun (xs, ys) point -> (point.x :: xs, point.y :: ys))
         ([], [])
    |> fun (x_values, y_values) ->
    List.iter (Printf.printf "%.2f\t") x_values;
    Printf.printf "\n";
    List.iter (Printf.printf "%.2f\t") y_values;
    Printf.printf "\n";
    Printf.printf "\n"

let is_sorted points =
  List.for_all2
    (fun p1 p2 -> p1.x <= p2.x)
    (points |> List.rev |> List.tl |> List.rev)
    (List.tl points)
  |> fun res ->
  match res with true -> points | false -> failwith "Points are not sorted"

let get_linear_interpolated_sequence x1 y1 x2 y2 step =
  let rec generate_sequence x acc =
    match x with
    | x when x > x2 -> acc
    | _ ->
        generate_sequence (x +. step)
          ({ x; y = y1 +. ((y2 -. y1) *. (x -. x1) /. (x2 -. x1)) } :: acc)
  in
  generate_sequence x1 []

let linear_interpolation points step =
  let points = List.rev points in
  match points with
  | p1 :: p2 :: _ ->
      let x1 = p2.x and y1 = p2.y in
      let x2 = p1.x and y2 = p1.y in
      get_linear_interpolated_sequence x1 y1 x2 y2 step
  | _ -> failwith "Invalid data format for interpolation"

(* let linear_interpolation points step = *)
(*  let points = List.rev points in *)
(*  match points with *)
(*  | [ p1; p2 ] -> ( *)
(*      let x1 = p2.x and y1 = p2.y in *)
(*      let x2 = p1.x and y2 = p1.y in *)
(*      match x2 -. x1 >= step with *)
(*      | false -> *)
(*          Printf.printf *)
(* "Step is too big for this interpolation diapason, enter more \ *)
   (*             points, please\n"; *)
(*          [] *)
(*      | true -> get_linear_interpolated_sequence x1 y1 x2 y2 step) *)
(*  | p1 :: p2 :: p3 :: _ -> ( *)
(*      let x1 = p3.x and y1 = p3.y in *)
(*      let x2 = p2.x and y2 = p2.y in *)
(*      let x3 = p1.x and y3 = p1.y in *)
(*      match x2 -. x1 >= step with *)
(*      | false -> ( *)
(*          match x3 -. x1 >= step with *)
(*          | false -> *)
(*              Printf.printf *)
(* "Step is too big for this interpolation diapason, enter more \ *)
   (*                 points, please\n"; *)
(*              [] *)
(*          | true -> *)
(*              let res1 = get_linear_interpolated_sequence x1 y1 x2 y2 step in *)
(*              let last_point = List.hd res1 in *)
(*              let res2 = *)
(*                get_linear_interpolated_sequence last_point.x last_point.y x3 y3 *)
(*                  step *)
(* (*                  incorrect merge btw*) *)
(*              in *)
(*              res2 @ res1) *)
(*      | true -> get_linear_interpolated_sequence x1 y1 x2 y2 step) *)
(*  | _ -> failwith "Invalid data format for interpolation" *)

let lagrange_polynomial points x =
  List.fold_left
    (fun res i ->
      List.fold_left
        (fun res j ->
          match i <> j with
          | true -> res *. (x -. j.x) /. (i.x -. j.x)
          | false -> res)
        1. points
      *. i.y
      +. res)
    0. points

let lagrange_interpolation points step =
  if List.length points < 4 then
    []
  else
    let limit =
      ((List.hd (List.rev points)).x -. (List.hd points).x) /. step
      |> ceil
      |> int_of_float
    in
    List.init (limit + 1) (fun i ->
        let x = (List.hd points).x +. (float_of_int i *. step) in
        if x <= (List.hd (List.rev points)).x then
          Some { x; y = lagrange_polynomial points x }
        else
          None)
    |> List.filter_map (fun x -> x)
    |> List.rev

let cut_window points =
  let rec take acc lst =
    match lst with
    | [] -> acc
    | h :: t when List.length acc < 4 -> take (h :: acc) t
    | _ :: t -> take acc t
  in
  take [] (List.rev points)

type interpolation_type = {
  name : string;
  required_points : int;
  interpolate : (point list -> float -> point list) list;
}

let interpolation_methods =
  [
    {
      name = "linear";
      required_points = 2;
      interpolate = [ linear_interpolation ];
    };
    {
      name = "lagrange";
      required_points = 4;
      interpolate = [ lagrange_interpolation ];
    };
    {
      name = "all";
      required_points = 2;
      interpolate = [ linear_interpolation; lagrange_interpolation ];
    };
  ]
