module Point = Shared.Point

module Rectangle : sig
  type t

  val width : t -> int
  val height : t -> int
  val area : t -> int
  val init : Point.t -> Point.t -> t
end = struct
  type t =
    { top_left : Point.t
    ; bottom_right : Point.t
    }

  let width t = t.bottom_right.j - t.top_left.j + 1
  let height t = t.bottom_right.i - t.top_left.i + 1
  let area t = width t * height t

  let init (a : Point.t) (b : Point.t) =
    let left_j = min a.j b.j
    and right_j = max a.j b.j
    and top_i = min a.i b.i
    and bot_i = max a.i b.i in
    { top_left = { i = top_i; j = left_j }; bottom_right = { i = bot_i; j = right_j } }
  ;;
end

let biggest_area_for_corner corner points =
  let rec aux best = function
    | [] -> best
    | point :: tail -> aux (max (Rectangle.area (Rectangle.init corner point)) best) tail
  in
  aux 1 points
;;

let biggest_area points =
  let rec aux best = function
    | [] -> best
    | corner :: points -> aux (max best (biggest_area_for_corner corner points)) points
  in
  aux 1 points
;;

let solve_part_one lines =
  let points = List.map Point.parse_xy lines in
  biggest_area points
;;

let solve_part_two _ = 0

let%expect_test "Solution Part One" =
  let solution = solve_part_one @@ Shared.read_input_lines "inputs/09.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 4729332959 |}]
;;

let%expect_test "Example Part One" =
  let solution =
    solve_part_one [ "7,1"; "11,1"; "11,7"; "9,7"; "9,5"; "2,5"; "2,3"; "7,3" ]
  in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 50 |}]
;;

let%expect_test "Solution Part Two" =
  let solution = solve_part_two @@ Shared.read_input_lines "inputs/09.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 0 |}]
;;

let%expect_test "Example Part Two" =
  let solution = solve_part_two [ (* Insert example input here *) ] in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 0 |}]
;;
