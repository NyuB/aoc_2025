module Point3D = struct
  type t =
    { x : int
    ; y : int
    ; z : int
    }

  let distance_2 a b =
    let dx = b.x - a.x
    and dy = b.y - a.y
    and dz = b.z - a.z in
    (dx * dx) + (dy * dy) + (dz * dz)
  ;;

  let compare a b =
    let by_x = Int.compare a.x b.x in
    if by_x != 0
    then by_x
    else (
      let by_y = Int.compare a.y b.y in
      if by_y != 0 then by_y else Int.compare a.z b.z)
  ;;

  let equal a b = a.x = b.x && a.y = b.y && a.z = b.z

  let parse line =
    match String.split_on_char ',' line with
    | [ x; y; z ] -> { x = int_of_string x; y = int_of_string y; z = int_of_string z }
    | _ -> failwith (Printf.sprintf "Invalid point format '%s'" line)
  ;;
end

module Point3DSet = Set.Make (Point3D)

module Point3DGraph = struct
  module Point3DMap = Map.Make (Point3D)

  type t = Point3DSet.t Point3DMap.t

  let init points =
    Point3DSet.fold
      (fun p graph -> Point3DMap.add p Point3DSet.empty graph)
      points
      Point3DMap.empty
  ;;

  let add_connection_unidir (t : t) (a : Point3D.t) (b : Point3D.t) : t =
    Point3DMap.update
      a
      (function
        | None -> Some (Point3DSet.singleton b)
        | Some already -> Some (Point3DSet.add b already))
      t
  ;;

  let add_connection (t : t) a b =
    let ab = add_connection_unidir t a b in
    add_connection_unidir ab b a
  ;;

  let are_connected (t : t) a b =
    Point3DMap.find_opt a t
    |> function
    | None -> false
    | Some bs -> Point3DSet.mem b bs
  ;;

  let vertices (t : t) = Point3DMap.to_seq t |> Seq.map fst |> Point3DSet.of_seq

  let edges t =
    Point3DMap.fold
      (fun _ connections total -> total + Point3DSet.cardinal connections)
      t
      0
  ;;

  let pick (set : Point3DSet.t) = Point3DSet.find_first (Fun.const true) set

  let rec circuit acc (t : t) start =
    let connections =
      Point3DMap.find start t |> Point3DSet.filter (fun p -> not @@ Point3DSet.mem p acc)
    in
    Point3DSet.fold
      (fun p connections -> Point3DSet.union connections (circuit connections t p))
      connections
      (Point3DSet.union connections acc)
  ;;

  let circuit t p = circuit (Point3DSet.singleton p) t p

  let circuits t =
    let rec aux acc t =
      if Point3DMap.is_empty t
      then acc
      else (
        let next_circuit = circuit t (pick (vertices t)) in
        let next_graph =
          Point3DMap.filter (fun p _ -> not @@ Point3DSet.mem p next_circuit) t
        in
        aux (next_circuit :: acc) next_graph)
    in
    aux [] t
  ;;
end

let all_connections_ascending points =
  Point3DSet.fold
    (fun a all ->
       Point3DSet.fold
         (fun b all ->
            if Point3D.equal a b
            then all
            else (
              let distance = Point3D.distance_2 a b in
              (a, b, distance) :: all))
         points
         all)
    points
    []
  |> List.sort (fun (_, _, da) (_, _, db) -> Int.compare da db)
;;

let rec add_n_best_connections graph n connections =
  if n = 0
  then graph
  else (
    match connections with
    | [] -> failwith "Unable to add a connection"
    | (a, b, _) :: tail ->
      if Point3DGraph.are_connected graph a b
      then add_n_best_connections graph n tail
      else add_n_best_connections (Point3DGraph.add_connection graph a b) (n - 1) tail)
;;

let solve_part_one n lines =
  let points = List.map Point3D.parse lines |> Point3DSet.of_list in
  let graph = Point3DGraph.init points in
  let connected = add_n_best_connections graph n (all_connections_ascending points) in
  Point3DGraph.circuits connected
  |> List.map Point3DSet.cardinal
  |> List.sort (fun a b -> -Int.compare a b)
  |> List.take 3
  |> List.fold_left Int.mul 1
;;

let solve_part_two _ = 0

let%expect_test "Solution Part One" =
  let solution = solve_part_one 1000 @@ Shared.read_input_lines "inputs/08.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 81536 |}]
;;

let%expect_test "Example Part One" =
  let solution =
    solve_part_one
      10
      [ "162,817,812"
      ; "57,618,57"
      ; "906,360,560"
      ; "592,479,940"
      ; "352,342,300"
      ; "466,668,158"
      ; "542,29,236"
      ; "431,825,988"
      ; "739,650,466"
      ; "52,470,668"
      ; "216,146,977"
      ; "819,987,18"
      ; "117,168,530"
      ; "805,96,715"
      ; "346,949,466"
      ; "970,615,88"
      ; "941,993,340"
      ; "862,61,35"
      ; "984,92,344"
      ; "425,690,689"
      ]
  in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 40 |}]
;;

let%expect_test "Solution Part Two" =
  let solution = solve_part_two @@ Shared.read_input_lines "inputs/08.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 0 |}]
;;

let%expect_test "Example Part Two" =
  let solution = solve_part_two [ (* Insert example input here *) ] in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 0 |}]
;;
