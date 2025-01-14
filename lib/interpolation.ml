type point = { x : float; y : float }

let print_interpolation_result points =
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
    (fun p1 p2 ->
      (*      Printf.printf "p1.x = %f, p2.x = %f\n" p1.x p2.x; *)
      p1.x <= p2.x)
    (points |> List.rev |> List.tl |> List.rev)
    (List.tl points)
  |> fun res ->
  match res with true -> points | false -> failwith "Points are not sorted"

let linear_interpolation points step =
  let points = List.rev points in
  match points with
  | p1 :: p2 :: _ ->
      let x1 = p2.x and y1 = p2.y in
      let x2 = p1.x and y2 = p1.y in
      let rec generate_sequence x acc =
        match x with
        | x when x >= x2 +. step -> acc
        | _ ->
            (*            Printf.printf "x = %f\n" x; *)
            generate_sequence (x +. step)
              ({ x; y = y1 +. ((y2 -. y1) *. (x -. x1) /. (x2 -. x1)) } :: acc)
      in
      generate_sequence x1 []
  | _ -> failwith "Invalid data format for interpolation"

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
  let limit =
    ((List.hd (List.rev points)).x -. (List.hd points).x) /. step
    |> ceil
    |> int_of_float
  in
  List.init (limit + 1) (fun i ->
      (List.hd points).x +. (float_of_int i *. step))
  |> List.map (fun x -> { x; y = lagrange_polynomial points x })
  |> List.rev

let cut_window points =
  let rec take acc lst =
    match lst with
    | [] -> acc
    | h :: t when List.length acc < 4 -> take (h :: acc) t
    | _ :: t -> take acc t
  in
  take [] (List.rev points)

let all = [ linear_interpolation; lagrange_interpolation ]
