module Point = Shared.Point

let debug = true
let debug s = if debug then print_endline s

module Segment : sig
  type t

  val init : Point.t -> Point.t -> t
  val intersect : t -> t -> bool
  val cross : t -> t -> bool
  val show : t -> string
  val equal : t -> t -> bool
  val end_ : t -> Point.t
  val merge : t -> t -> t option
  val on : t -> Point.t -> bool
end = struct
  type t =
    { start : Point.t
    ; end_ : Point.t
    }

  let init (start : Point.t) (end_ : Point.t) : t =
    assert (start.i = end_.i || start.j = end_.j)
    (* Purely vertical or purely horizontal *);
    { start; end_ }
  ;;

  let end_ t = t.end_
  let equal a b = Point.equal a.start b.start && Point.equal a.end_ b.end_

  type orientation =
    | Vertical
    | Horizontal

  let show_orientation = function
    | Vertical -> "↑"
    | Horizontal -> "←→"
  ;;

  let orientation t = if t.start.j = t.end_.j then Vertical else Horizontal

  let show t =
    Printf.sprintf
      "%s %s %s"
      (Point.show t.start)
      (show_orientation @@ orientation t)
      (Point.show t.end_)
  ;;

  let rec intersect a b =
    match orientation a, orientation b with
    | Vertical, Vertical | Horizontal, Horizontal ->
      debug (Printf.sprintf "Checking [%s] against [%s]" (show a) (show b));
      false
    | Vertical, Horizontal ->
      debug (Printf.sprintf "Checking [%s] against [%s]" (show a) (show b));
      let a_high_i = min a.start.i a.end_.i
      and a_low_i = max a.start.i a.end_.i
      and a_j = a.start.j
      and b_left_j = min b.start.j b.end_.j
      and b_right_j = max b.start.j b.end_.j
      and b_i = b.start.i in
      debug
        (Printf.sprintf
           "    (%d %d %d) (%d %d %d)"
           a_high_i
           a_low_i
           a_j
           b_left_j
           b_right_j
           b_i);
      a_high_i <= b_i && a_low_i >= b_i && b_left_j <= a_j && b_right_j >= a_j
    | Horizontal, Vertical -> intersect b a
  ;;

  let flip_p (p : Point.t) = Point.{ i = p.j; j = p.i }
  let flip t = { start = flip_p t.start; end_ = flip_p t.end_ }

  let rec cross a b =
    match orientation a, orientation b with
    | Vertical, Vertical -> cross (flip a) (flip b)
    | Horizontal, Horizontal ->
      if a.start.i != b.start.i || equal a b
      then false
      else (
        let a_left_j = min a.start.j a.end_.j in
        let a_right_j = max a.start.j a.end_.j in
        let b_left_j = min b.start.j b.end_.j in
        let b_right_j = max b.start.j b.end_.j in
        List.exists
          Fun.id
          [ a_left_j < b_left_j && a_right_j > b_left_j
          ; a_left_j >= b_left_j && a_left_j < b_right_j && a_right_j > b_right_j
          ])
    | Vertical, Horizontal ->
      let a_high_i = min a.start.i a.end_.i
      and a_low_i = max a.start.i a.end_.i
      and a_j = a.start.j
      and b_left_j = min b.start.j b.end_.j
      and b_right_j = max b.start.j b.end_.j
      and b_i = b.start.i in
      a_high_i < b_i && a_low_i > b_i && b_left_j < a_j && b_right_j > a_j
    | Horizontal, Vertical -> cross b a
  ;;

  let merge a b =
    match orientation a, orientation b with
    | Vertical, Vertical | Horizontal, Horizontal ->
      if Point.equal a.end_ b.start then Some (init a.start b.end_) else None
    | _ -> None
  ;;

  let on t (p : Point.t) =
    match orientation t with
    | Vertical ->
      p.j = t.start.j && min t.start.i t.end_.i <= p.i && max t.start.i t.end_.i >= p.i
    | Horizontal ->
      p.i = t.start.i && min t.start.j t.end_.j <= p.j && max t.start.j t.end_.j >= p.j
  ;;
end

module Rectangle : sig
  type t

  val width : t -> int
  val height : t -> int
  val area : t -> int
  val init : Point.t -> Point.t -> t
  val segments : t -> Segment.t list
  val corners : t -> Point.t list
  val top_left : t -> Point.t
end = struct
  type t =
    { top_left : Point.t
    ; bottom_right : Point.t
    }

  let width t = t.bottom_right.j - t.top_left.j + 1
  let height t = t.bottom_right.i - t.top_left.i + 1
  let area t = width t * height t
  let top_left t = t.top_left

  let init (a : Point.t) (b : Point.t) =
    let left_j = min a.j b.j
    and right_j = max a.j b.j
    and top_i = min a.i b.i
    and bot_i = max a.i b.i in
    { top_left = { i = top_i; j = left_j }; bottom_right = { i = bot_i; j = right_j } }
  ;;

  let corners t =
    let top_right = Point.{ i = t.top_left.i; j = t.bottom_right.j } in
    let bot_left = Point.{ i = t.bottom_right.i; j = t.top_left.j } in
    [ t.top_left; top_right; t.bottom_right; bot_left ]
  ;;

  let segments t =
    let top_right = Point.{ i = t.top_left.i; j = t.bottom_right.j } in
    let bot_left = Point.{ i = t.bottom_right.i; j = t.top_left.j } in
    [ Segment.init t.top_left top_right
    ; Segment.init top_right t.bottom_right
    ; Segment.init t.bottom_right bot_left
    ; Segment.init bot_left t.top_left
    ]
  ;;
end

module Polygon = struct
  type t = Segment.t list

  let make_segments points =
    let rec aux segments = function
      | [] ->
        List.rev
          (Segment.init (Segment.end_ (List.hd segments)) (List.hd points) :: segments)
      | point :: tail ->
        aux (Segment.init (Segment.end_ (List.hd segments)) point :: segments) tail
    in
    aux [ Segment.init (List.nth points 0) (List.nth points 1) ] (List.drop 2 points)
  ;;

  let merge_segments segments =
    let rec aux merged = function
      | [] -> List.rev merged
      | segment :: tail ->
        (match merged with
         | [] -> aux [ segment ] tail
         | prev :: seg_tail ->
           (match Segment.merge prev segment with
            | None -> aux (segment :: merged) tail
            | Some s -> aux (s :: seg_tail) tail))
    in
    aux [] segments
  ;;

  let init points : t = points |> make_segments |> merge_segments
  let parse_ij lines = lines |> List.map Point.parse_ij |> init
  let parse_xy lines = lines |> List.map Point.parse_xy |> init
  let point_on (t : t) p = List.exists (fun side -> Segment.on side p) t

  let point_inside (t : t) (p : Point.t) =
    point_on t p
    ||
    let segment_from_outside = Segment.init { i = -2; j = p.j } p in
    List.fold_left
      (fun inside side ->
         if Segment.intersect segment_from_outside side
         then (
           debug "    X";
           not inside)
         else (
           debug "    O";
           inside))
      false
      t
  ;;

  let point_inside_rect t rectangle =
    let top_left = Rectangle.top_left rectangle in
    let p = Point.{ i = top_left.i + 1; j = top_left.j + 1 } in
    point_inside t p
  ;;

  let segment_cross t segment =
    List.exists
      (fun s ->
         let crossed = Segment.cross segment s in
         if crossed
         then (
           debug
             (Printf.sprintf "    %s crossed %s" (Segment.show segment) (Segment.show s));
           true)
         else false)
      t
  ;;

  let rectangle_intersects t rectangle =
    List.exists (segment_cross t) (Rectangle.segments rectangle)
  ;;

  (** Assumes [rectangle] corners are on [t]'s sides*)
  let rectangle_inside (t : t) rectangle =
    (not @@ rectangle_intersects t rectangle) && point_inside_rect t rectangle
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

let rectangles_from_corner corner points =
  let rec aux acc = function
    | [] -> acc
    | point :: tail -> aux (Rectangle.init corner point :: acc) tail
  in
  aux [] points
;;

let all_rectangles points =
  let rec aux acc = function
    | [] -> List.concat acc
    | corner :: points -> aux (rectangles_from_corner corner points :: acc) points
  in
  aux [] points
;;

let solve_part_one lines =
  let points = List.map Point.parse_xy lines in
  biggest_area points
;;

let solve_part_two lines =
  let points = List.map Point.parse_xy lines in
  let polygon = Polygon.init points in
  all_rectangles points
  |> List.filter (Polygon.rectangle_inside polygon)
  |> List.map Rectangle.area
  |> Shared.Reduction.MaxInt.reduce_list
;;

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

(* let%expect_test "Solution Part Two" =
  let solution = solve_part_two @@ Shared.read_input_lines "inputs/09.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 2436968301 |}]
;; *)

let%expect_test "Example Part Two" =
  let solution =
    solve_part_two [ "7,1"; "11,1"; "11,7"; "9,7"; "9,5"; "2,5"; "2,3"; "7,3" ]
  in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect
    {|
    Checking [(-2, 3) ↑ (4, 3)] against [(1, 7) ←→ (1, 11)]
        (-2 4 3) (7 11 1)
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(1, 11) ↑ (7, 11)]
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(7, 11) ←→ (7, 9)]
        (-2 4 3) (9 11 7)
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(7, 9) ↑ (5, 9)]
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(5, 9) ←→ (5, 2)]
        (-2 4 3) (2 9 5)
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(5, 2) ↑ (3, 2)]
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(3, 2) ←→ (3, 7)]
        (-2 4 3) (2 7 3)
        X
    Checking [(-2, 3) ↑ (4, 3)] against [(3, 7) ↑ (1, 7)]
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(1, 7) ←→ (1, 11)]
        (-2 4 3) (7 11 1)
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(1, 11) ↑ (7, 11)]
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(7, 11) ←→ (7, 9)]
        (-2 4 3) (9 11 7)
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(7, 9) ↑ (5, 9)]
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(5, 9) ←→ (5, 2)]
        (-2 4 3) (2 9 5)
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(5, 2) ↑ (3, 2)]
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(3, 2) ←→ (3, 7)]
        (-2 4 3) (2 7 3)
        X
    Checking [(-2, 3) ↑ (4, 3)] against [(3, 7) ↑ (1, 7)]
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(1, 7) ←→ (1, 11)]
        (-2 4 3) (7 11 1)
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(1, 11) ↑ (7, 11)]
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(7, 11) ←→ (7, 9)]
        (-2 4 3) (9 11 7)
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(7, 9) ↑ (5, 9)]
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(5, 9) ←→ (5, 2)]
        (-2 4 3) (2 9 5)
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(5, 2) ↑ (3, 2)]
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(3, 2) ←→ (3, 7)]
        (-2 4 3) (2 7 3)
        X
    Checking [(-2, 3) ↑ (4, 3)] against [(3, 7) ↑ (1, 7)]
        O
    Checking [(-2, 8) ↑ (4, 8)] against [(1, 7) ←→ (1, 11)]
        (-2 4 8) (7 11 1)
        X
    Checking [(-2, 8) ↑ (4, 8)] against [(1, 11) ↑ (7, 11)]
        O
    Checking [(-2, 8) ↑ (4, 8)] against [(7, 11) ←→ (7, 9)]
        (-2 4 8) (9 11 7)
        O
    Checking [(-2, 8) ↑ (4, 8)] against [(7, 9) ↑ (5, 9)]
        O
    Checking [(-2, 8) ↑ (4, 8)] against [(5, 9) ←→ (5, 2)]
        (-2 4 8) (2 9 5)
        O
    Checking [(-2, 8) ↑ (4, 8)] against [(5, 2) ↑ (3, 2)]
        O
    Checking [(-2, 8) ↑ (4, 8)] against [(3, 2) ←→ (3, 7)]
        (-2 4 8) (2 7 3)
        O
    Checking [(-2, 8) ↑ (4, 8)] against [(3, 7) ↑ (1, 7)]
        O
        (3, 2) ←→ (3, 9) crossed (3, 2) ←→ (3, 7)
    Checking [(-2, 3) ↑ (6, 3)] against [(1, 7) ←→ (1, 11)]
        (-2 6 3) (7 11 1)
        O
    Checking [(-2, 3) ↑ (6, 3)] against [(1, 11) ↑ (7, 11)]
        O
    Checking [(-2, 3) ↑ (6, 3)] against [(7, 11) ←→ (7, 9)]
        (-2 6 3) (9 11 7)
        O
    Checking [(-2, 3) ↑ (6, 3)] against [(7, 9) ↑ (5, 9)]
        O
    Checking [(-2, 3) ↑ (6, 3)] against [(5, 9) ←→ (5, 2)]
        (-2 6 3) (2 9 5)
        X
    Checking [(-2, 3) ↑ (6, 3)] against [(5, 2) ↑ (3, 2)]
        O
    Checking [(-2, 3) ↑ (6, 3)] against [(3, 2) ←→ (3, 7)]
        (-2 6 3) (2 7 3)
        X
    Checking [(-2, 3) ↑ (6, 3)] against [(3, 7) ↑ (1, 7)]
        O
        (3, 9) ↑ (7, 9) crossed (7, 9) ↑ (5, 9)
        (3, 2) ←→ (3, 9) crossed (3, 2) ←→ (3, 7)
    Checking [(-2, 3) ↑ (6, 3)] against [(1, 7) ←→ (1, 11)]
        (-2 6 3) (7 11 1)
        O
    Checking [(-2, 3) ↑ (6, 3)] against [(1, 11) ↑ (7, 11)]
        O
    Checking [(-2, 3) ↑ (6, 3)] against [(7, 11) ←→ (7, 9)]
        (-2 6 3) (9 11 7)
        O
    Checking [(-2, 3) ↑ (6, 3)] against [(7, 9) ↑ (5, 9)]
        O
    Checking [(-2, 3) ↑ (6, 3)] against [(5, 9) ←→ (5, 2)]
        (-2 6 3) (2 9 5)
        X
    Checking [(-2, 3) ↑ (6, 3)] against [(5, 2) ↑ (3, 2)]
        O
    Checking [(-2, 3) ↑ (6, 3)] against [(3, 2) ←→ (3, 7)]
        (-2 6 3) (2 7 3)
        X
    Checking [(-2, 3) ↑ (6, 3)] against [(3, 7) ↑ (1, 7)]
        O
    Checking [(-2, 10) ↑ (6, 10)] against [(1, 7) ←→ (1, 11)]
        (-2 6 10) (7 11 1)
        X
    Checking [(-2, 10) ↑ (6, 10)] against [(1, 11) ↑ (7, 11)]
        O
    Checking [(-2, 10) ↑ (6, 10)] against [(7, 11) ←→ (7, 9)]
        (-2 6 10) (9 11 7)
        O
    Checking [(-2, 10) ↑ (6, 10)] against [(7, 9) ↑ (5, 9)]
        O
    Checking [(-2, 10) ↑ (6, 10)] against [(5, 9) ←→ (5, 2)]
        (-2 6 10) (2 9 5)
        O
    Checking [(-2, 10) ↑ (6, 10)] against [(5, 2) ↑ (3, 2)]
        O
    Checking [(-2, 10) ↑ (6, 10)] against [(3, 2) ←→ (3, 7)]
        (-2 6 10) (2 7 3)
        O
    Checking [(-2, 10) ↑ (6, 10)] against [(3, 7) ↑ (1, 7)]
        O
        (7, 11) ←→ (7, 7) crossed (7, 11) ←→ (7, 9)
        (3, 2) ←→ (3, 11) crossed (3, 2) ←→ (3, 7)
        (5, 2) ←→ (5, 11) crossed (5, 9) ←→ (5, 2)
    Checking [(-2, 10) ↑ (6, 10)] against [(1, 7) ←→ (1, 11)]
        (-2 6 10) (7 11 1)
        X
    Checking [(-2, 10) ↑ (6, 10)] against [(1, 11) ↑ (7, 11)]
        O
    Checking [(-2, 10) ↑ (6, 10)] against [(7, 11) ←→ (7, 9)]
        (-2 6 10) (9 11 7)
        O
    Checking [(-2, 10) ↑ (6, 10)] against [(7, 9) ↑ (5, 9)]
        O
    Checking [(-2, 10) ↑ (6, 10)] against [(5, 9) ←→ (5, 2)]
        (-2 6 10) (2 9 5)
        O
    Checking [(-2, 10) ↑ (6, 10)] against [(5, 2) ↑ (3, 2)]
        O
    Checking [(-2, 10) ↑ (6, 10)] against [(3, 2) ←→ (3, 7)]
        (-2 6 10) (2 7 3)
        O
    Checking [(-2, 10) ↑ (6, 10)] against [(3, 7) ↑ (1, 7)]
        O
    Checking [(-2, 10) ↑ (8, 10)] against [(1, 7) ←→ (1, 11)]
        (-2 8 10) (7 11 1)
        X
    Checking [(-2, 10) ↑ (8, 10)] against [(1, 11) ↑ (7, 11)]
        O
    Checking [(-2, 10) ↑ (8, 10)] against [(7, 11) ←→ (7, 9)]
        (-2 8 10) (9 11 7)
        X
    Checking [(-2, 10) ↑ (8, 10)] against [(7, 9) ↑ (5, 9)]
        O
    Checking [(-2, 10) ↑ (8, 10)] against [(5, 9) ←→ (5, 2)]
        (-2 8 10) (2 9 5)
        O
    Checking [(-2, 10) ↑ (8, 10)] against [(5, 2) ↑ (3, 2)]
        O
    Checking [(-2, 10) ↑ (8, 10)] against [(3, 2) ←→ (3, 7)]
        (-2 8 10) (2 7 3)
        O
    Checking [(-2, 10) ↑ (8, 10)] against [(3, 7) ↑ (1, 7)]
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(1, 7) ←→ (1, 11)]
        (-2 2 8) (7 11 1)
        X
    Checking [(-2, 8) ↑ (2, 8)] against [(1, 11) ↑ (7, 11)]
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(7, 11) ←→ (7, 9)]
        (-2 2 8) (9 11 7)
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(7, 9) ↑ (5, 9)]
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(5, 9) ←→ (5, 2)]
        (-2 2 8) (2 9 5)
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(5, 2) ↑ (3, 2)]
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(3, 2) ←→ (3, 7)]
        (-2 2 8) (2 7 3)
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(3, 7) ↑ (1, 7)]
        O
        (1, 2) ←→ (1, 11) crossed (1, 7) ←→ (1, 11)
        (1, 2) ←→ (1, 11) crossed (1, 7) ←→ (1, 11)
    Checking [(-2, 10) ↑ (2, 10)] against [(1, 7) ←→ (1, 11)]
        (-2 2 10) (7 11 1)
        X
    Checking [(-2, 10) ↑ (2, 10)] against [(1, 11) ↑ (7, 11)]
        O
    Checking [(-2, 10) ↑ (2, 10)] against [(7, 11) ←→ (7, 9)]
        (-2 2 10) (9 11 7)
        O
    Checking [(-2, 10) ↑ (2, 10)] against [(7, 9) ↑ (5, 9)]
        O
    Checking [(-2, 10) ↑ (2, 10)] against [(5, 9) ←→ (5, 2)]
        (-2 2 10) (2 9 5)
        O
    Checking [(-2, 10) ↑ (2, 10)] against [(5, 2) ↑ (3, 2)]
        O
    Checking [(-2, 10) ↑ (2, 10)] against [(3, 2) ←→ (3, 7)]
        (-2 2 10) (2 7 3)
        O
    Checking [(-2, 10) ↑ (2, 10)] against [(3, 7) ↑ (1, 7)]
        O
        (7, 9) ↑ (1, 9) crossed (7, 9) ↑ (5, 9)
    Checking [(-2, 12) ↑ (2, 12)] against [(1, 7) ←→ (1, 11)]
        (-2 2 12) (7 11 1)
        O
    Checking [(-2, 12) ↑ (2, 12)] against [(1, 11) ↑ (7, 11)]
        O
    Checking [(-2, 12) ↑ (2, 12)] against [(7, 11) ←→ (7, 9)]
        (-2 2 12) (9 11 7)
        O
    Checking [(-2, 12) ↑ (2, 12)] against [(7, 9) ↑ (5, 9)]
        O
    Checking [(-2, 12) ↑ (2, 12)] against [(5, 9) ←→ (5, 2)]
        (-2 2 12) (2 9 5)
        O
    Checking [(-2, 12) ↑ (2, 12)] against [(5, 2) ↑ (3, 2)]
        O
    Checking [(-2, 12) ↑ (2, 12)] against [(3, 2) ←→ (3, 7)]
        (-2 2 12) (2 7 3)
        O
    Checking [(-2, 12) ↑ (2, 12)] against [(3, 7) ↑ (1, 7)]
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(1, 7) ←→ (1, 11)]
        (-2 2 8) (7 11 1)
        X
    Checking [(-2, 8) ↑ (2, 8)] against [(1, 11) ↑ (7, 11)]
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(7, 11) ←→ (7, 9)]
        (-2 2 8) (9 11 7)
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(7, 9) ↑ (5, 9)]
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(5, 9) ←→ (5, 2)]
        (-2 2 8) (2 9 5)
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(5, 2) ↑ (3, 2)]
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(3, 2) ←→ (3, 7)]
        (-2 2 8) (2 7 3)
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(3, 7) ↑ (1, 7)]
        O
    Checking [(-2, 3) ↑ (2, 3)] against [(1, 7) ←→ (1, 11)]
        (-2 2 3) (7 11 1)
        O
    Checking [(-2, 3) ↑ (2, 3)] against [(1, 11) ↑ (7, 11)]
        O
    Checking [(-2, 3) ↑ (2, 3)] against [(7, 11) ←→ (7, 9)]
        (-2 2 3) (9 11 7)
        O
    Checking [(-2, 3) ↑ (2, 3)] against [(7, 9) ↑ (5, 9)]
        O
    Checking [(-2, 3) ↑ (2, 3)] against [(5, 9) ←→ (5, 2)]
        (-2 2 3) (2 9 5)
        O
    Checking [(-2, 3) ↑ (2, 3)] against [(5, 2) ↑ (3, 2)]
        O
    Checking [(-2, 3) ↑ (2, 3)] against [(3, 2) ←→ (3, 7)]
        (-2 2 3) (2 7 3)
        O
    Checking [(-2, 3) ↑ (2, 3)] against [(3, 7) ↑ (1, 7)]
        O
        (1, 7) ↑ (5, 7) crossed (3, 7) ↑ (1, 7)
        (5, 7) ↑ (1, 7) crossed (3, 7) ↑ (1, 7)
        (1, 9) ↑ (7, 9) crossed (7, 9) ↑ (5, 9)
        (7, 11) ←→ (7, 7) crossed (7, 11) ←→ (7, 9)
    Checking [(-2, 8) ↑ (2, 8)] against [(1, 7) ←→ (1, 11)]
        (-2 2 8) (7 11 1)
        X
    Checking [(-2, 8) ↑ (2, 8)] against [(1, 11) ↑ (7, 11)]
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(7, 11) ←→ (7, 9)]
        (-2 2 8) (9 11 7)
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(7, 9) ↑ (5, 9)]
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(5, 9) ←→ (5, 2)]
        (-2 2 8) (2 9 5)
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(5, 2) ↑ (3, 2)]
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(3, 2) ←→ (3, 7)]
        (-2 2 8) (2 7 3)
        O
    Checking [(-2, 8) ↑ (2, 8)] against [(3, 7) ↑ (1, 7)]
        O
    18
    |}]
;;

let parse_segment_ij sa sb = Segment.init (Point.parse_ij sa) (Point.parse_ij sb)

let print_intersect ((sa, sb), (sc, sd)) =
  let s1 = parse_segment_ij sa sb
  and s2 = parse_segment_ij sc sd in
  let intersect = Segment.intersect s1 s2 in
  let msg = if intersect then "intersect" else "do not intersect" in
  print_endline (Printf.sprintf "%s and %s (%s)" (Segment.show s1) (Segment.show s2) msg)
;;

let%expect_test "Intersections" =
  List.iter
    print_intersect
    [ ("5,0", "5,10"), ("0,11", "10,11")
    ; ("5,0", "5,10"), ("0,5", "10,5")
    ; ("5,0", "5,10"), ("5,10", "5,11")
    ; ("5,0", "5,10"), ("5,9", "5,11") (* a--b--a--b *)
    ; ("5,0", "5,10"), ("5,0", "5,11") (* ab--a--b *)
    ; ("0,5", "10,5"), ("9,5", "11,5") (* a--b--a--b *)
    ; ("0,5", "10,5"), ("0,5", "11,5") (* ab--a--b *)
    ];
  [%expect
    {|
    Checking [(0, 11) ↑ (10, 11)] against [(5, 0) ←→ (5, 10)]
        (0 10 11) (0 10 5)
    (5, 0) ←→ (5, 10) and (0, 11) ↑ (10, 11) (do not intersect)
    Checking [(0, 5) ↑ (10, 5)] against [(5, 0) ←→ (5, 10)]
        (0 10 5) (0 10 5)
    (5, 0) ←→ (5, 10) and (0, 5) ↑ (10, 5) (intersect)
    Checking [(5, 0) ←→ (5, 10)] against [(5, 10) ←→ (5, 11)]
    (5, 0) ←→ (5, 10) and (5, 10) ←→ (5, 11) (do not intersect)
    Checking [(5, 0) ←→ (5, 10)] against [(5, 9) ←→ (5, 11)]
    (5, 0) ←→ (5, 10) and (5, 9) ←→ (5, 11) (do not intersect)
    Checking [(5, 0) ←→ (5, 10)] against [(5, 0) ←→ (5, 11)]
    (5, 0) ←→ (5, 10) and (5, 0) ←→ (5, 11) (do not intersect)
    Checking [(0, 5) ↑ (10, 5)] against [(9, 5) ↑ (11, 5)]
    (0, 5) ↑ (10, 5) and (9, 5) ↑ (11, 5) (do not intersect)
    Checking [(0, 5) ↑ (10, 5)] against [(0, 5) ↑ (11, 5)]
    (0, 5) ↑ (10, 5) and (0, 5) ↑ (11, 5) (do not intersect)
    |}]
;;

let%expect_test "Polygon" =
  let polygon = Polygon.parse_ij [ "0,0"; "0,5"; "5,5"; "5,0" ] in
  List.iter (Fun.compose print_endline Segment.show) polygon;
  [%expect
    {|
    (0, 0) ←→ (0, 5)
    (0, 5) ↑ (5, 5)
    (5, 5) ←→ (5, 0)
    (5, 0) ↑ (0, 0)
    |}];
  List.iter
    (fun p ->
       let p = Point.parse_ij p in
       print_endline
         (Printf.sprintf
            "%s %s"
            (Point.show p)
            (if Polygon.point_inside polygon p then "(in)" else "(out)")))
    [ "1,1"; "2,2"; "0,0"; "0,1"; "0,2"; "0,5"; "3,6"; "6,1"; "3,5" ];
  [%expect
    {|
    Checking [(-2, 1) ↑ (1, 1)] against [(0, 0) ←→ (0, 5)]
        (-2 1 1) (0 5 0)
        X
    Checking [(-2, 1) ↑ (1, 1)] against [(0, 5) ↑ (5, 5)]
        O
    Checking [(-2, 1) ↑ (1, 1)] against [(5, 5) ←→ (5, 0)]
        (-2 1 1) (0 5 5)
        O
    Checking [(-2, 1) ↑ (1, 1)] against [(5, 0) ↑ (0, 0)]
        O
    (1, 1) (in)
    Checking [(-2, 2) ↑ (2, 2)] against [(0, 0) ←→ (0, 5)]
        (-2 2 2) (0 5 0)
        X
    Checking [(-2, 2) ↑ (2, 2)] against [(0, 5) ↑ (5, 5)]
        O
    Checking [(-2, 2) ↑ (2, 2)] against [(5, 5) ←→ (5, 0)]
        (-2 2 2) (0 5 5)
        O
    Checking [(-2, 2) ↑ (2, 2)] against [(5, 0) ↑ (0, 0)]
        O
    (2, 2) (in)
    (0, 0) (in)
    (0, 1) (in)
    (0, 2) (in)
    (0, 5) (in)
    Checking [(-2, 6) ↑ (3, 6)] against [(0, 0) ←→ (0, 5)]
        (-2 3 6) (0 5 0)
        O
    Checking [(-2, 6) ↑ (3, 6)] against [(0, 5) ↑ (5, 5)]
        O
    Checking [(-2, 6) ↑ (3, 6)] against [(5, 5) ←→ (5, 0)]
        (-2 3 6) (0 5 5)
        O
    Checking [(-2, 6) ↑ (3, 6)] against [(5, 0) ↑ (0, 0)]
        O
    (3, 6) (out)
    Checking [(-2, 1) ↑ (6, 1)] against [(0, 0) ←→ (0, 5)]
        (-2 6 1) (0 5 0)
        X
    Checking [(-2, 1) ↑ (6, 1)] against [(0, 5) ↑ (5, 5)]
        O
    Checking [(-2, 1) ↑ (6, 1)] against [(5, 5) ←→ (5, 0)]
        (-2 6 1) (0 5 5)
        X
    Checking [(-2, 1) ↑ (6, 1)] against [(5, 0) ↑ (0, 0)]
        O
    (6, 1) (out)
    (3, 5) (in)
    |}];
  let rectangle = Rectangle.init (Point.parse_ij "0,0") (Point.parse_ij "5,5") in
  print_endline (if Polygon.rectangle_inside polygon rectangle then "(in)" else "(out)");
  [%expect
    {|
    Checking [(-2, 1) ↑ (1, 1)] against [(0, 0) ←→ (0, 5)]
        (-2 1 1) (0 5 0)
        X
    Checking [(-2, 1) ↑ (1, 1)] against [(0, 5) ↑ (5, 5)]
        O
    Checking [(-2, 1) ↑ (1, 1)] against [(5, 5) ←→ (5, 0)]
        (-2 1 1) (0 5 5)
        O
    Checking [(-2, 1) ↑ (1, 1)] against [(5, 0) ↑ (0, 0)]
        O
    (in)
    |}]
;;

let%expect_test "Polygon" =
  let polygon = Polygon.parse_ij [ "0,0"; "0,2"; "0,5"; "5,5"; "5,0" ] in
  List.iter (Fun.compose print_endline Segment.show) polygon;
  [%expect
    {|
    (0, 0) ←→ (0, 5)
    (0, 5) ↑ (5, 5)
    (5, 5) ←→ (5, 0)
    (5, 0) ↑ (0, 0)
    |}];
  List.iter
    (fun p ->
       let p = Point.parse_ij p in
       print_endline
         (Printf.sprintf
            "%s %s"
            (Point.show p)
            (if Polygon.point_inside polygon p then "(in)" else "(out)")))
    [ "1,1"; "2,2"; "0,0"; "0,1"; "0,2"; "0,5"; "3,6"; "6,1"; "3,5" ];
  [%expect
    {|
    Checking [(-2, 1) ↑ (1, 1)] against [(0, 0) ←→ (0, 5)]
        (-2 1 1) (0 5 0)
        X
    Checking [(-2, 1) ↑ (1, 1)] against [(0, 5) ↑ (5, 5)]
        O
    Checking [(-2, 1) ↑ (1, 1)] against [(5, 5) ←→ (5, 0)]
        (-2 1 1) (0 5 5)
        O
    Checking [(-2, 1) ↑ (1, 1)] against [(5, 0) ↑ (0, 0)]
        O
    (1, 1) (in)
    Checking [(-2, 2) ↑ (2, 2)] against [(0, 0) ←→ (0, 5)]
        (-2 2 2) (0 5 0)
        X
    Checking [(-2, 2) ↑ (2, 2)] against [(0, 5) ↑ (5, 5)]
        O
    Checking [(-2, 2) ↑ (2, 2)] against [(5, 5) ←→ (5, 0)]
        (-2 2 2) (0 5 5)
        O
    Checking [(-2, 2) ↑ (2, 2)] against [(5, 0) ↑ (0, 0)]
        O
    (2, 2) (in)
    (0, 0) (in)
    (0, 1) (in)
    (0, 2) (in)
    (0, 5) (in)
    Checking [(-2, 6) ↑ (3, 6)] against [(0, 0) ←→ (0, 5)]
        (-2 3 6) (0 5 0)
        O
    Checking [(-2, 6) ↑ (3, 6)] against [(0, 5) ↑ (5, 5)]
        O
    Checking [(-2, 6) ↑ (3, 6)] against [(5, 5) ←→ (5, 0)]
        (-2 3 6) (0 5 5)
        O
    Checking [(-2, 6) ↑ (3, 6)] against [(5, 0) ↑ (0, 0)]
        O
    (3, 6) (out)
    Checking [(-2, 1) ↑ (6, 1)] against [(0, 0) ←→ (0, 5)]
        (-2 6 1) (0 5 0)
        X
    Checking [(-2, 1) ↑ (6, 1)] against [(0, 5) ↑ (5, 5)]
        O
    Checking [(-2, 1) ↑ (6, 1)] against [(5, 5) ←→ (5, 0)]
        (-2 6 1) (0 5 5)
        X
    Checking [(-2, 1) ↑ (6, 1)] against [(5, 0) ↑ (0, 0)]
        O
    (6, 1) (out)
    (3, 5) (in)
    |}]
;;

let%expect_test "Example polygon" =
  let polygon =
    Polygon.parse_xy [ "7,1"; "11,1"; "11,7"; "9,7"; "9,5"; "2,5"; "2,3"; "7,3" ]
  in
  let rectangle = Rectangle.init (Point.parse_ij "3,2") (Point.parse_ij "5,9") in
  print_endline (if Polygon.rectangle_inside polygon rectangle then "(in)" else "(out)");
  [%expect
    {|
        (3, 2) ←→ (3, 9) crossed (3, 2) ←→ (3, 7)
    (out)
    |}];
  print_endline (if Polygon.point_inside_rect polygon rectangle then "(in)" else "(out)");
  [%expect
    {|
    Checking [(-2, 3) ↑ (4, 3)] against [(1, 7) ←→ (1, 11)]
        (-2 4 3) (7 11 1)
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(1, 11) ↑ (7, 11)]
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(7, 11) ←→ (7, 9)]
        (-2 4 3) (9 11 7)
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(7, 9) ↑ (5, 9)]
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(5, 9) ←→ (5, 2)]
        (-2 4 3) (2 9 5)
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(5, 2) ↑ (3, 2)]
        O
    Checking [(-2, 3) ↑ (4, 3)] against [(3, 2) ←→ (3, 7)]
        (-2 4 3) (2 7 3)
        X
    Checking [(-2, 3) ↑ (4, 3)] against [(3, 7) ↑ (1, 7)]
        O
    (in)
    |}];
  List.iter
    (fun (a, b) ->
       let seg = Segment.init (Point.parse_ij a) (Point.parse_ij b) in
       print_endline
         (if Polygon.segment_cross polygon seg
          then Printf.sprintf "%s (cross)" (Segment.show seg)
          else Printf.sprintf "%s (no cross)" (Segment.show seg)))
    [ "3,2", "5,2"; "3,2", "3,9" ];
  [%expect
    {|
    (3, 2) ↑ (5, 2) (no cross)
        (3, 2) ←→ (3, 9) crossed (3, 2) ←→ (3, 7)
    (3, 2) ←→ (3, 9) (cross)
    |}]
;;
