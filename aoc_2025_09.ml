module Point = Shared.Point

type turn =
  | Left
  | Right

module Segment = struct
  (** 'Manhattan' segments *)

  type t =
    { start : Point.t
    ; end_ : Point.t
    }

  let init (start : Point.t) (end_ : Point.t) : t =
    (* Purely vertical or purely horizontal *)
    assert (start.i = end_.i || start.j = end_.j);
    { start; end_ }
  ;;

  let end_ t = t.end_
  let equal a b = Point.equal a.start b.start && Point.equal a.end_ b.end_

  type orientation =
    | North
    | South
    | East
    | West

  let is_vertical t = t.start.j = t.end_.j
  let is_horizontal t = t.start.i = t.end_.i

  let orientation t =
    if is_vertical t
    then if t.start.i < t.end_.i then South else North
    else if t.start.j < t.end_.j
    then East
    else West
  ;;

  let turn a b =
    assert (Point.equal a.end_ b.start);
    match orientation a, orientation b with
    | North, North
    | South, South
    | East, East
    | West, West
    | North, South
    | South, North
    | East, West
    | West, East -> failwith "No turn between to consecutive segments"
    | North, West | South, East | East, North | West, South -> Left
    | North, East | South, West | East, South | West, North -> Right
  ;;

  let merge a b =
    assert (Point.equal a.end_ b.start);
    if (is_vertical a && is_vertical b) || (is_horizontal a && is_horizontal b)
    then Some (init a.start b.end_)
    else None
  ;;
end

module Rectangle : sig
  type t

  val width : t -> int
  val height : t -> int
  val area : t -> int
  val init : Point.t -> Point.t -> t
  val top_left : t -> Point.t
  val bottom_right : t -> Point.t
  val intersecting_area : t -> t -> int
end = struct
  type t =
    { top_left : Point.t
    ; bottom_right : Point.t
    }

  let width t = t.bottom_right.j - t.top_left.j + 1
  let height t = t.bottom_right.i - t.top_left.i + 1
  let area t = width t * height t
  let top_left t = t.top_left
  let bottom_right t = t.bottom_right

  let init (a : Point.t) (b : Point.t) =
    let left_j = min a.j b.j
    and right_j = max a.j b.j
    and top_i = min a.i b.i
    and bot_i = max a.i b.i in
    { top_left = { i = top_i; j = left_j }; bottom_right = { i = bot_i; j = right_j } }
  ;;

  let intersecting_area a b =
    let top_left_i = max a.top_left.i b.top_left.i in
    let top_left_j = max a.top_left.j b.top_left.j in
    let bot_right_i = min a.bottom_right.i b.bottom_right.i in
    let bot_right_j = min a.bottom_right.j b.bottom_right.j in
    if top_left_i > bot_right_i || top_left_j > bot_right_j
    then 0
    else
      area
        (init
           Point.{ i = top_left_i; j = top_left_j }
           Point.{ i = bot_right_i; j = bot_right_j })
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

  let interior_side (t : t) =
    let rec aux (left, right) = function
      | [] -> failwith "Unreachable"
      | [ _ ] -> left, right
      | prev :: next :: tail ->
        (match Segment.turn prev next with
         | Left -> aux (left + 1, right) (next :: tail)
         | Right -> aux (left, right + 1) (next :: tail))
    in
    let left, right = aux (0, 0) t in
    if left > right then Left else Right
  ;;
end

module SweepLine = struct
  (** A 'sweep line' algoritmh to divide a polygon into horizontal stripes 
Sweeps from the leftmost (lowest 'j') segments
*)

  (** Encountering a segment with polarity [Open] means 'entering into' the swept polygon,
  and encountering a polarity [Close] means 'going out from' the swept polygon *)
  type polarity =
    | Open
    | Close

  (** The segments accumulated along the sweeping loop *)
  module VerticalSegment = struct
    type t =
      { top : int
      ; bot : int
      ; j : int
      ; polarity : polarity
      }

    (** 
        @param s must be vertical
        @param open_orientation orientation of the [Open] polarity segments
    *)
    let of_segment (s : Segment.t) (open_orientation : Segment.orientation) =
      assert (Segment.is_vertical s);
      { top = min s.start.i s.end_.i
      ; bot = max s.start.i s.end_.i
      ; j = s.start.j
      ; polarity = (if Segment.orientation s = open_orientation then Open else Close)
      }
    ;;

    let leftmost_ordering a b = Int.compare a.j b.j

    (** [truncate_if_already_open t opened] is [t] with endpoints removed 
    if any already opened segment among [opened] has a common endpoints *)
    let rec truncate_if_already_opened (t : t) (opened : t list) =
      match opened with
      | [] -> t
      | opened :: tail ->
        if opened.bot = t.top
        then truncate_if_already_opened { t with top = t.top + 1 } tail
        else if opened.top = t.bot
        then truncate_if_already_opened { t with bot = t.bot - 1 } tail
        else truncate_if_already_opened t tail
    ;;

    (** [close_and_split opening closing] is
    - [None] if [closing] does not overlap with [opening]
    - [Some (closed_area, remaining_opened)] otherwise, where [closed_area] is the stripe area delimited by [opening], [closing]
       and [remaining_opened] are the sub-segments from [opening] that were not covered by [closing] *)
    let close_and_split opening closing =
      if opening.bot <= closing.top || opening.top >= closing.bot
      then None
      else (
        let top =
          if opening.top < closing.top
          then
            [ { top = opening.top; bot = closing.top; j = opening.j; polarity = Open } ]
          else []
        in
        let bot =
          if opening.bot > closing.bot
          then
            [ { top = closing.bot; bot = opening.bot; j = opening.j; polarity = Open } ]
          else []
        in
        let closed =
          truncate_if_already_opened
            { top = max opening.top closing.top
            ; bot = min opening.bot closing.bot
            ; j = closing.j
            ; polarity = Open
            }
            (top @ bot)
        in
        let closed_area =
          Rectangle.init
            { i = max closed.top closing.top; j = opening.j }
            { i = min closed.bot closing.bot; j = closing.j }
        in
        Some (closed_area, top @ bot))
    ;;
  end

  type t =
    { open_orientation : Segment.orientation
    ; segments : VerticalSegment.t list
    }

  let init_segments open_orientation segments =
    List.fold_left
      (fun acc s ->
         if Segment.is_horizontal s
         then acc
         else VerticalSegment.of_segment s open_orientation :: acc)
      []
      segments
    |> List.sort VerticalSegment.leftmost_ordering
  ;;

  let of_polygon (p : Polygon.t) : t =
    let open_orientation =
      match Polygon.interior_side p with
      | Left -> Segment.South
      | Right -> Segment.North
    in
    let segments = init_segments open_orientation p in
    { open_orientation; segments }
  ;;

  let rec sweep_loop stripes opened segments =
    match segments with
    | [] ->
      assert (opened = []);
      stripes
    | (VerticalSegment.{ polarity = Open; _ } as s) :: tail ->
      sweep_loop
        stripes
        (VerticalSegment.truncate_if_already_opened s opened :: opened)
        tail
    | (VerticalSegment.{ polarity = Close; _ } as s) :: tail ->
      let next_areas, next_opened =
        List.fold_left
          (fun (next_areas, next_opened) opened ->
             match VerticalSegment.close_and_split opened s with
             | None -> next_areas, opened :: next_opened
             | Some (area, split) -> area :: next_areas, List.rev_append split next_opened)
          ([], [])
          opened
      in
      sweep_loop (List.rev_append next_areas stripes) next_opened tail
  ;;

  (** [divide_polygon_into_stripes p] returns a list of disjoint rectangles composing the polygon [p] *)
  let divide_polygon_into_stripes p =
    let t = of_polygon p in
    sweep_loop [] [] t.segments
  ;;
end

module Solver_Two = struct
  type t =
    { original_rectangle : Rectangle.t
    ; cropped_area : int
    }

  let of_rectangle original_rectangle =
    { original_rectangle; cropped_area = Rectangle.area original_rectangle }
  ;;

  let crop { original_rectangle; cropped_area } cropping =
    { original_rectangle
    ; cropped_area =
        cropped_area - Rectangle.intersecting_area original_rectangle cropping
    }
  ;;

  let crop_by_each rectangles t = List.fold_left crop t rectangles

  (** [rectangle_inside polygon rectangles] are the [rectangles] that are fully inside [polygon] *)
  let rectangle_inside polygon rectangles =
    let polygon_stripes = SweepLine.divide_polygon_into_stripes polygon in
    let only_rectangles_cropped_to_zero =
      List.map (Fun.compose (crop_by_each polygon_stripes) of_rectangle) rectangles
      |> List.filter (fun { cropped_area; _ } -> cropped_area = 0)
    in
    List.map (fun t -> t.original_rectangle) only_rectangles_cropped_to_zero
  ;;

  let solve polygon rectangles =
    rectangle_inside polygon rectangles
    |> List.fold_left
         (fun best rectangle -> max best (Rectangle.area rectangle))
         Int.min_int
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
  let rectangles = all_rectangles points in
  let polygon = Polygon.init points in
  Solver_Two.solve polygon rectangles
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

let%expect_test "Solution Part Two" =
  let solution = solve_part_two @@ Shared.read_input_lines "inputs/09.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 1474477524 |}]
;;

let%expect_test "Example Part Two" =
  let solution =
    solve_part_two [ "7,1"; "11,1"; "11,7"; "9,7"; "9,5"; "2,5"; "2,3"; "7,3" ]
  in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 24 |}]
;;

(* Visual test/debugging *)
type grid = char array array

let height (grid : grid) = Array.length grid
let width (grid : grid) = if height grid = 0 then 0 else Array.length grid.(0)
let string_of_char_array = Fun.compose String.of_seq Array.to_seq

let print_grid (grid : grid) =
  Array.iter (Fun.compose print_endline string_of_char_array) grid
;;

let write_rectangle (grid : grid) (rectangle : Rectangle.t) (repr : char) =
  for i = (Rectangle.top_left rectangle).i to (Rectangle.bottom_right rectangle).i do
    for j = (Rectangle.top_left rectangle).j to (Rectangle.bottom_right rectangle).j do
      grid.(i).(j) <- repr
    done
  done
;;

let min_segment_i (s : Segment.t) = min s.start.i s.end_.i
let max_segment_i (s : Segment.t) = max s.start.i s.end_.i
let min_segment_j (s : Segment.t) = min s.start.j s.end_.j
let max_segment_j (s : Segment.t) = max s.start.j s.end_.j

let write_segment (grid : grid) (segment : Segment.t) (repr : char) =
  for i = min_segment_i segment to max_segment_i segment do
    for j = min_segment_j segment to max_segment_j segment do
      grid.(i).(j) <- repr
    done
  done
;;

let repr_of_int base i = Char.chr (Char.code base + i)
let repr_of_int i = if i >= 26 then repr_of_int 'A' i else repr_of_int 'a' i

let print_rectangles (rectangles : Rectangle.t list) =
  let max_i, max_j =
    List.fold_left
      (fun (max_i, max_j) rectangle ->
         ( max max_i (Rectangle.bottom_right rectangle).i
         , max max_j (Rectangle.bottom_right rectangle).j ))
      (Int.min_int, Int.min_int)
      rectangles
  in
  let grid = Array.make_matrix (max_i + 1) (max_j + 1) '.' in
  List.iteri (fun i r -> write_rectangle grid r (repr_of_int i)) rectangles;
  print_grid grid
;;

let print_polygon (polygon : Polygon.t) =
  let max_i, max_j =
    List.fold_left
      (fun (max_i, max_j) segment ->
         max max_i (max_segment_i segment), max max_j (max_segment_j segment))
      (Int.min_int, Int.min_int)
      polygon
  in
  let grid = Array.make_matrix (max_i + 1) (max_j + 1) '.' in
  List.iter (fun s -> write_segment grid s '*') polygon;
  print_grid grid
;;

let%expect_test "Polygon stripes" =
  let points =
    List.map Point.parse_xy [ "7,1"; "11,1"; "11,7"; "9,7"; "9,5"; "2,5"; "2,3"; "7,3" ]
  in
  let polygon = Polygon.init points in
  print_polygon polygon;
  [%expect
    {|
    ............
    .......*****
    .......*...*
    ..******...*
    ..*........*
    ..********.*
    .........*.*
    .........***
    |}];
  let stripes = SweepLine.divide_polygon_into_stripes polygon in
  print_rectangles stripes;
  [%expect
    {|
    ............
    .......bbbbb
    .......bbbbb
    ..cccccccccc
    ..cccccccccc
    ..cccccccccc
    .........aaa
    .........aaa
    |}];
  List.iter
    (fun s ->
       print_rectangles [ s ];
       print_endline "---")
    stripes;
  [%expect
    {|
    ............
    ............
    ............
    ............
    ............
    ............
    .........aaa
    .........aaa
    ---
    ............
    .......aaaaa
    .......aaaaa
    ---
    ............
    ............
    ............
    ..aaaaaaaaaa
    ..aaaaaaaaaa
    ..aaaaaaaaaa
    ---
    |}]
;;

let polygon_stripe_test points =
  let points = List.map Point.parse_xy points in
  let polygon = Polygon.init points in
  let stripes = SweepLine.divide_polygon_into_stripes polygon in
  print_polygon polygon;
  print_endline "";
  print_endline "---";
  print_rectangles stripes;
  print_endline "---";
  List.iter
    (fun s ->
       print_rectangles [ s ];
       print_endline "---")
    stripes;
  print_endline ""
;;

let%expect_test "Stripes" =
  polygon_stripe_test [ "0,0"; "0,5"; "1,5"; "1,3"; "2,3"; "2,0" ];
  [%expect
    {|
    ***
    *.*
    *.*
    ***
    **.
    **.

    ---
    aaa
    aaa
    aaa
    aaa
    bb.
    bb.
    ---
    aaa
    aaa
    aaa
    aaa
    ---
    ..
    ..
    ..
    ..
    aa
    aa
    ---
    |}];
  polygon_stripe_test
    [ "10,10"
    ; "15,10"
    ; "15,15"
    ; "20,15"
    ; "20,20"
    ; "15,20"
    ; "15,25"
    ; "10,25"
    ; "10,20"
    ; "5,20"
    ; "5,15"
    ; "10,15"
    ];
  [%expect
    {|
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    ..........******.....
    ..........*....*.....
    ..........*....*.....
    ..........*....*.....
    ..........*....*.....
    .....******....******
    .....*..............*
    .....*..............*
    .....*..............*
    .....*..............*
    .....******....******
    ..........*....*.....
    ..........*....*.....
    ..........*....*.....
    ..........*....*.....
    ..........******.....

    ---
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    ..........bbbbbb.....
    ..........bbbbbb.....
    ..........bbbbbb.....
    ..........bbbbbb.....
    ..........bbbbbb.....
    .....aaaaaaaaaaaaaaaa
    .....aaaaaaaaaaaaaaaa
    .....aaaaaaaaaaaaaaaa
    .....aaaaaaaaaaaaaaaa
    .....aaaaaaaaaaaaaaaa
    .....aaaaaaaaaaaaaaaa
    ..........cccccc.....
    ..........cccccc.....
    ..........cccccc.....
    ..........cccccc.....
    ..........cccccc.....
    ---
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....................
    .....aaaaaaaaaaaaaaaa
    .....aaaaaaaaaaaaaaaa
    .....aaaaaaaaaaaaaaaa
    .....aaaaaaaaaaaaaaaa
    .....aaaaaaaaaaaaaaaa
    .....aaaaaaaaaaaaaaaa
    ---
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ..........aaaaaa
    ..........aaaaaa
    ..........aaaaaa
    ..........aaaaaa
    ..........aaaaaa
    ---
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ................
    ..........aaaaaa
    ..........aaaaaa
    ..........aaaaaa
    ..........aaaaaa
    ..........aaaaaa
    ---
    |}]
;;
