module Point = struct
  type t =
    { i : int
    ; j : int
    }

  let compare { i = ia; j = ja } { i = ib; j = jb } =
    let compare_i = Int.compare ia ib in
    if compare_i != 0 then compare_i else Int.compare ja jb
  ;;

  let show { i; j } = Printf.sprintf "(%d, %d)" i j
  let left { i; j } = { i; j = j - 1 }
  let right { i; j } = { i; j = j + 1 }
  let equal a b = a.i = b.i && a.j = b.j
  let hash a = Int.hash (a.i * 13 * a.j * 17)
end

module Splitters : sig
  type t

  val of_list : Point.t list -> t

  (** `split splitters beam` returns Some [(splitted_beams, splitted_on)] if the `beam` hits one of the `splitters`, [None] otherwise  *)
  val split : t -> Point.t -> (Point.t list * Point.t) option
end = struct
  type t = Point.t list

  (* Sort points by highest row first *)
  let of_list points = List.sort Point.compare points

  let rec split (t : t) (beam : Point.t) : (Point.t list * Point.t) option =
    match t with
    | [] -> None
    | splitter :: tail ->
      if splitter.j == beam.j && splitter.i >= beam.i
      then Some (Point.[ left splitter; right splitter ], splitter)
      else split tail beam
  ;;
end

module PointSet = Set.Make (Point)

let parse_input lines : Point.t * Splitters.t =
  let beam = ref Point.{ i = -1; j = -1 }
  and splitters = Dynarray.create () in
  List.iteri
    (fun i line ->
       String.iteri
         (fun j c ->
            match c with
            | '^' -> Dynarray.add_last splitters Point.{ i; j }
            | 'S' -> beam := { i; j }
            | _ -> ())
         line)
    lines;
  assert (!beam.i >= 0);
  !beam, Splitters.of_list (Dynarray.to_list splitters)
;;

let rec solve_one all_splitters_hit beams splitters =
  let splitted_beams, splitters_hit =
    PointSet.fold
      (fun beam ((splitted_beams, splitters_hit) as current) ->
         match Splitters.split splitters beam with
         | None -> current
         | Some (s, splitted_on) ->
           ( PointSet.union splitted_beams (PointSet.of_list s)
           , PointSet.add splitted_on splitters_hit ))
      beams
      (PointSet.empty, PointSet.empty)
  in
  if PointSet.is_empty splitters_hit
  then PointSet.cardinal all_splitters_hit
  else solve_one (PointSet.union all_splitters_hit splitters_hit) splitted_beams splitters
;;

module Memo = Hashtbl.Make (Point)

(** [timelines_from splitters memo splitter] counts the number of timelines generated after hitting [splitter]. Memoized on [splitter] in [memo] *)
let rec timelines_from splitters memo splitter =
  let recurse = timelines_from splitters memo in
  if Memo.mem memo splitter
  then Memo.find memo splitter
  else (
    let recurse_on_fork fork =
      match Splitters.split splitters fork with
      | Some (_, next_splitter) -> recurse next_splitter
      | None -> 1
    in
    let result =
      recurse_on_fork (Point.left splitter) + recurse_on_fork (Point.right splitter)
    in
    Memo.add memo splitter result;
    result)
;;

let solve_one beam splitters =
  solve_one PointSet.empty (PointSet.of_list [ beam ]) splitters
;;

let solve_part_one lines =
  let beam, splitters = parse_input lines in
  solve_one beam splitters
;;

let solve_two timeline splitters =
  match Splitters.split splitters timeline with
  | None -> 1 (* A single boring timeline *)
  | Some (_, splitter) ->
    (* Enter the multiverse *)
    let memo = Memo.create 16 in
    timelines_from splitters memo splitter
;;

let solve_part_two lines =
  let beam, splitters = parse_input lines in
  solve_two beam splitters
;;

let%expect_test "Solution Part One" =
  let solution = solve_part_one @@ Shared.read_input_lines "inputs/07.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 1635 |}]
;;

let%expect_test "Solution Part Two" =
  let solution = solve_part_two @@ Shared.read_input_lines "inputs/07.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 58097428661390 |}]
;;

let example_input =
  [ ".......S......."
  ; "..............."
  ; ".......^......."
  ; "..............."
  ; "......^.^......"
  ; "..............."
  ; ".....^.^.^....."
  ; "..............."
  ; "....^.^...^...."
  ; "..............."
  ; "...^.^...^.^..."
  ; "..............."
  ; "..^...^.....^.."
  ; "..............."
  ; ".^.^.^.^.^...^."
  ; "..............."
  ]
;;

let%expect_test "Example Part One" =
  let solution = solve_part_one example_input in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 21 |}]
;;

let%expect_test "Example Part Two" =
  let solution = solve_part_two example_input in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 40 |}]
;;
