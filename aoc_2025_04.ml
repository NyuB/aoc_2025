type cell =
  | Empty
  | Roll

let parse_cell = function
  | '@' -> Roll
  | '.' -> Empty
  | c -> failwith (Printf.sprintf "Invalid cell %c" c)
;;

let show_cell = function
  | Roll -> "@"
  | Empty -> "."
;;

module Grid = struct
  type 'a t = cell array array

  let parse lines : cell t =
    List.map (fun l -> l |> String.to_seq |> Seq.map parse_cell |> Array.of_seq) lines
    |> Array.of_list
  ;;

  let height t = Array.length t
  let width t = if height t = 0 then 0 else Array.length t.(0)
  let copy (t : 'a t) : 'a t = Array.init (height t) (fun i -> Array.copy t.(i))
  let get (t : 'a t) (i, j) = t.(i).(j)

  let get_opt (t : 'a t) (i, j) =
    if i < 0 || i >= height t || j < 0 || j >= width t then None else Some (get t (i, j))
  ;;

  let positions (t : 'a t) =
    let rec aux acc i j =
      if i >= height t
      then List.rev acc
      else if j >= width t
      then aux acc (i + 1) 0
      else aux ((i, j) :: acc) i (j + 1)
    in
    aux [] 0 0
  ;;

  let map t f =
    let res = copy t in
    positions t |> List.iter (fun (i, j) -> res.(i).(j) <- f (i, j) (get t (i, j)));
    res
  ;;

  let count (t : 'a t) (eq : 'a -> 'a -> bool) (elem : 'a) =
    positions t |> List.map (get t) |> List.filter (eq elem) |> List.length
  ;;

  let neighbours_8 t (i, j) =
    [ i - 1, j - 1
    ; i, j - 1
    ; i + 1, j - 1
    ; i - 1, j
    ; i + 1, j
    ; i - 1, j + 1
    ; i, j + 1
    ; i + 1, j + 1
    ]
    |> List.filter_map (get_opt t)
  ;;
end

let remove_removable_rolls (grid : cell Grid.t) : cell Grid.t =
  Grid.map grid (fun (i, j) cell ->
    if cell = Empty
    then cell
    else (
      let roll_neighbours_count =
        Grid.neighbours_8 grid (i, j) |> List.filter (( = ) Roll) |> List.length
      in
      if roll_neighbours_count < 4 then Empty else Roll))
;;

let count_rolls grid = Grid.count grid ( = ) Roll

let solve_part_one lines =
  let grid = Grid.parse lines in
  let removed = remove_removable_rolls grid in
  count_rolls grid - count_rolls removed
;;

let solve_part_two lines =
  let grid = Grid.parse lines in
  let original = count_rolls grid in
  let rec count_removed grid =
    let before = count_rolls grid in
    let removed = remove_removable_rolls grid in
    let after = count_rolls removed in
    if before = after then original - after else count_removed removed
  in
  count_removed grid
;;

let%expect_test "Solution Part One" =
  print_endline
    (Printf.sprintf "%d" (solve_part_one (Shared.read_input_lines "inputs/04.txt")));
  [%expect {| 1351 |}]
;;

let%expect_test "Example Part One" =
  print_endline
    (Printf.sprintf
       "%d"
       (solve_part_one
          [ "..@@.@@@@."
          ; "@@@.@.@.@@"
          ; "@@@@@.@.@@"
          ; "@.@@@@..@."
          ; "@@.@@@@.@@"
          ; ".@@@@@@@.@"
          ; ".@.@.@.@@@"
          ; "@.@@@.@@@@"
          ; ".@@@@@@@@."
          ; "@.@.@@@.@."
          ]));
  [%expect {| 13 |}]
;;

let%expect_test "Solution Part Two" =
  print_endline
    (Printf.sprintf "%d" (solve_part_two (Shared.read_input_lines "inputs/04.txt")));
  [%expect {| 8345 |}]
;;

let%expect_test "Example Part Two" =
  print_endline
    (Printf.sprintf
       "%d"
       (solve_part_two
          [ "..@@.@@@@."
          ; "@@@.@.@.@@"
          ; "@@@@@.@.@@"
          ; "@.@@@@..@."
          ; "@@.@@@@.@@"
          ; ".@@@@@@@.@"
          ; ".@.@.@.@@@"
          ; "@.@@@.@@@@"
          ; ".@@@@@@@@."
          ; "@.@.@@@.@."
          ]));
  [%expect {| 43 |}]
;;

let%expect_test "Neighbours" =
  let grid =
    Grid.parse
      [ "..@@.@@@@."
      ; "@@@.@.@.@@"
      ; "@@@@@.@.@@"
      ; "@.@@@@..@."
      ; "@@.@@@@.@@"
      ; ".@@@@@@@.@"
      ; ".@.@.@.@@@"
      ; "@.@@@.@@@@"
      ; ".@@@@@@@@."
      ; "@.@.@@@.@."
      ]
  in
  List.iter (Fun.compose print_endline show_cell) (Grid.neighbours_8 grid (9, 9));
  [%expect
    {|
    @
    @
    .
    |}]
;;
