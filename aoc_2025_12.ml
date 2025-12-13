module Point = Shared.Point

let copy_matrix mat = Array.map Array.copy mat

module Grid = struct
  (** [t[i,j]] is [true] if the position [i,j] is free, false otherwise *)
  type t = bool array array

  let height (t : t) = Array.length t
  let width (t : t) = Array.length t.(0)
  let is_free t (p : Point.t) = t.(p.i).(p.j)
  let block t (p : Point.t) = t.(p.i).(p.j) <- false

  let place (t : t) (points : Point.t array) : t option =
    let exception Impossible in
    let copy = copy_matrix t in
    try
      Array.iter
        (fun p -> if is_free copy p then block copy p else raise Impossible)
        points;
      Some copy
    with
    | Impossible -> None
  ;;

  let parse w_x_h =
    match String.split_on_char 'x' w_x_h with
    | [ width; heigt ] ->
      Array.make_matrix (int_of_string heigt) (int_of_string width) true
    | _ -> failwith (Printf.sprintf "Invalid grid dimensions '%s'" w_x_h)
  ;;
end

module Shape = struct
  type t =
    { width : int
    ; height : int
    ; points : Point.t array
    }

  let rotate_clockwise { width; height; points } =
    let rotated_points =
      Array.map (fun Point.{ i; j } -> Point.{ j = height - i - 1; i = j }) points
    in
    Array.sort Point.compare rotated_points;
    { width = height; height = width; points = rotated_points }
  ;;

  let flip_vertical { width; height; points } =
    let flipped_points =
      Array.map (fun Point.{ i; j } -> Point.{ i = height - 1 - i; j }) points
    in
    Array.sort Point.compare flipped_points;
    { width; height; points = flipped_points }
  ;;

  let flip_horizontal { width; height; points } =
    let flipped_points =
      Array.map (fun Point.{ i; j } -> Point.{ i; j = height - 1 - j }) points
    in
    Array.sort Point.compare flipped_points;
    { width; height; points = flipped_points }
  ;;

  let all_rotations t =
    [ t
    ; rotate_clockwise t
    ; rotate_clockwise @@ rotate_clockwise t
    ; rotate_clockwise @@ rotate_clockwise @@ rotate_clockwise t
    ]
  ;;

  let equal a b =
    a.width = b.width && a.height = b.height && Array.equal Point.equal a.points b.points
  ;;

  let add_if_not_mem list elem =
    if List.exists (equal elem) list then list else elem :: list
  ;;

  let all_arrangements t =
    all_rotations t
    |> List.concat_map (fun t -> [ t; flip_horizontal t; flip_vertical t ])
    |> List.fold_left add_if_not_mem []
  ;;

  let placements_on_grid t (min_i, min_j) grid =
    let rec aux acc i j =
      if i + t.height > Grid.height grid
      then acc
      else if j + t.width > Grid.width grid
      then aux acc (i + 1) 0
      else (
        let offset_points =
          Array.map (fun (p : Point.t) -> Point.{ i = p.i + i; j = p.j + j }) t.points
        in
        match Grid.place grid offset_points with
        | None -> aux acc i (j + 1)
        | Some placed -> aux ((placed, (i, j)) :: acc) i (j + 1))
    in
    aux [] min_i min_j
  ;;

  let parse lines =
    let width = ref 0
    and height = ref 0 in
    let points = Dynarray.create () in
    let rec aux i = function
      | [] ->
        height := i;
        let points = Dynarray.to_array points in
        Array.sort Point.compare points;
        { width = !width; height = !height; points }
      | line :: tail ->
        width := String.length line;
        String.iteri
          (fun j c -> if c = '#' then Dynarray.add_last points Point.{ i; j })
          line;
        aux (i + 1) tail
    in
    aux 0 lines
  ;;

  let to_printable_array t =
    Array.init_matrix t.height t.width (fun i j ->
      if Array.exists (fun (p : Point.t) -> p.i = i && p.j = j) t.points then '#' else '.')
  ;;
end

let rec solve grid shapes_and_counts =
  (* print_endline
    (Printf.sprintf "Solving '%s'" (String.concat " " (List.map string_of_int counts))); *)
  match shapes_and_counts with
  | [] -> true
  | (_, 0) :: tail -> solve grid tail
  | ((rotations, (min_i, min_j)), n) :: tail ->
    (* print_endline (Printf.sprintf "    %d possible rotations" (List.length rotations)); *)
    let forks =
      List.concat_map
        (fun shape -> Shape.placements_on_grid shape (min_i, min_j) grid)
        rotations
    in
    (* print_endline (Printf.sprintf "    %d possible placements" (List.length forks)); *)
    let exception Solved in
    (try
       List.iter
         (fun (grid, (min_i, min_j)) ->
            if solve grid (((rotations, (min_i, min_j)), n - 1) :: tail) then raise Solved)
         forks;
       false
     with
     | Solved -> true)
;;

let split_on_string str l =
  let rec aux acc current = function
    | [] -> List.rev (List.rev current :: acc)
    | s :: tail when String.equal s str -> aux (List.rev current :: acc) [] tail
    | s :: tail -> aux acc (s :: current) tail
  in
  aux [] [] l
;;

let parse_shape block = Shape.parse (List.tl block) (* skip index line *)

let parse_area_and_counts line =
  match String.split_on_char ':' line with
  | [ wh; counts ] ->
    let grid = Grid.parse wh
    and counts =
      String.trim counts |> String.split_on_char ' ' |> List.map int_of_string
    in
    grid, counts
  | _ -> failwith (Printf.sprintf "Invalid area line '%s'" line)
;;

let shape_size l =
  let any_shape : Shape.t = List.hd l in
  Array.length any_shape.points
;;

let solve_part_one _ = 0
(* let blocks = split_on_string "" lines |> List.rev in
  let areas = List.hd blocks
  and shapes = List.tl blocks |> List.map parse_shape in
  List.fold_left
    (fun (total, iterations) area ->
       if iterations > 2 then failwith "Oops";
       let grid, counts = parse_area_and_counts area in
       let shapes_and_counts =
         List.combine (List.map Shape.all_arrangements shapes) counts
         |> List.sort (fun (sa, _) (sb, _) -> Int.compare (shape_size sb) (shape_size sa))
         |> List.map (fun (rotations, count) -> (rotations, (0, 0)), count)
       in
       let total_available = Grid.width grid * Grid.height grid
       and total_required =
         shapes_and_counts
         |> List.map (fun ((rotations, _), count) -> count * shape_size rotations)
         |> Shared.Reduction.AddInt.reduce_list
       in
       if total_available >= total_required && solve grid shapes_and_counts
       then total + 1, iterations + 1
       else total, iterations + 1)
    (0, 0)
    areas
  |> fst *)

let solve_part_two _ = 0

let example_input =
  [ "0:"
  ; "###"
  ; "##."
  ; "##."
  ; ""
  ; "1:"
  ; "###"
  ; "##."
  ; ".##"
  ; ""
  ; "2:"
  ; ".##"
  ; "###"
  ; "##."
  ; ""
  ; "3:"
  ; "##."
  ; "###"
  ; "##."
  ; ""
  ; "4:"
  ; "###"
  ; "#.."
  ; "###"
  ; ""
  ; "5:"
  ; "###"
  ; ".#."
  ; "###"
  ; ""
  ; "4x4: 0 0 0 0 2 0"
  ; "12x5: 1 0 1 0 2 2"
  ; "12x5: 1 0 1 0 3 2"
  ]
;;

let%expect_test "Solution Part One" =
  let solution = solve_part_one @@ Shared.read_input_lines "inputs/12.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 0 |}]
;;

let%expect_test "Example Part One" =
  let solution = solve_part_one example_input in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 0 |}]
;;

let%expect_test "Solution Part Two" =
  let solution = solve_part_two @@ Shared.read_input_lines "inputs/12.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 0 |}]
;;

let%expect_test "Example Part Two" =
  let solution = solve_part_two [ (* Insert example input here *) ] in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 0 |}]
;;

let print_shape shape =
  shape
  |> Shape.to_printable_array
  |> Array.map (Fun.compose String.of_seq Array.to_seq)
  |> Array.iter print_endline
;;

let print_all_arrangements shape =
  Shape.all_arrangements shape
  |> List.iter (fun shape ->
    print_endline "---";
    print_shape shape);
  print_endline "---"
;;

let%expect_test "Shapes" =
  let cross = Shape.parse [ ".#."; "###"; ".#." ] in
  print_all_arrangements cross;
  [%expect
    {|
    ---
    .#.
    ###
    .#.
    ---
    |}];
  let angle = Shape.parse [ "###"; "#.."; "..." ] in
  print_all_arrangements angle;
  [%expect
    {|
    ---
    #..
    #..
    ##.
    ---
    ...
    ..#
    ###
    ---
    ..#
    ..#
    .##
    ---
    ##.
    #..
    #..
    ---
    .##
    ..#
    ..#
    ---
    ...
    #..
    ###
    ---
    ###
    ..#
    ...
    ---
    ###
    #..
    ...
    ---
    |}];
  let corner = Shape.parse [ "###"; "###"; "##." ] in
  print_all_arrangements corner;
  [%expect
    {|
    ---
    .##
    ###
    ###
    ---
    ##.
    ###
    ###
    ---
    ###
    ###
    .##
    ---
    ###
    ###
    ##.
    ---
    |}]
;;
