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

module Point3DCircuits : sig
  (** a graph of points connected within non-intersecting circuits *)
  type t

  (** [init points] is a graph with each point of [points] in its own lonely circuit *)
  val init : Point3DSet.t -> t

  (** [merge_circuits t a b] merge the circuits of a and b, creating a bigger circuit *)
  val merge_circuits : t -> Point3D.t -> Point3D.t -> t

  (** [cardinal t] is the number of points in the graph [t] *)
  val cardinal : t -> int

  (** [cardinal_circuit t p] is the number of points (including [p]) in the circuit of [p] *)
  val cardinal_circuit : t -> Point3D.t -> int

  (** [circuits t] is the list of all non-intersecting circuits in [t] *)
  val circuits : t -> Point3DSet.t list
end = struct
  module Point3DMap = Map.Make (Point3D)

  type circuit =
    | Circuit of Point3DSet.t
    | Merged_with of Point3D.t

  type t = circuit Point3DMap.t

  let init (points : Point3DSet.t) : t =
    Point3DSet.fold
      (fun p graph -> Point3DMap.add p (Circuit (Point3DSet.singleton p)) graph)
      points
      Point3DMap.empty
  ;;

  let rec find_actual_circuit (t : t) p : Point3D.t * Point3DSet.t =
    match Point3DMap.find p t with
    | Circuit c -> p, c
    | Merged_with another -> find_actual_circuit t another
  ;;

  let merge_circuits (t : t) (a : Point3D.t) (b : Point3D.t) : t =
    let actual_a, circuit_a = find_actual_circuit t a
    and actual_b, circuit_b = find_actual_circuit t b in
    if Point3D.equal actual_a actual_b
    then t
    else (
      let merged = Point3DSet.union circuit_a circuit_b in
      Point3DMap.add_seq
        (List.to_seq [ actual_a, Circuit merged; actual_b, Merged_with actual_a ])
        t)
  ;;

  let cardinal (t : t) = Point3DMap.cardinal t

  let cardinal_circuit (t : t) p =
    let _, c = find_actual_circuit t p in
    Point3DSet.cardinal c
  ;;

  let circuits (t : t) : Point3DSet.t list =
    Point3DMap.to_seq t
    |> Seq.filter_map (function
      | _, Circuit c -> Some c
      | _ -> None)
    |> List.of_seq
  ;;
end

let one_on_two l =
  let rec aux acc = function
    | [] -> List.rev acc
    | [ _ ] -> failwith "Invalid argument: list length is not even"
    | a :: _ :: tail -> aux (a :: acc) tail
  in
  aux [] l
;;

type connection = Point3D.t * Point3D.t * int

(** [all_connections_ascending points] is the list of all pairwise connections (undirected)
sorted by ascending distance between the connection points *)
let all_connections_ascending points : connection list =
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
  (* Skip duplicates a <-> b, b <-> a which are side-by-side because we just sorted them *)
  |> one_on_two
;;

(** [connect_n circuits n connections] is the resulting graph obtained after applying the first [n] connections in [connections] *)
let rec connect_n (circuits : Point3DCircuits.t) n (connections : connection list)
  : Point3DCircuits.t
  =
  if n = 0
  then circuits
  else (
    match connections with
    | [] -> failwith "Unable to add a connection"
    | (a, b, _) :: tail ->
      connect_n (Point3DCircuits.merge_circuits circuits a b) (n - 1) tail)
;;

(** [connect_until_single_circuit circuits connections] is the last connection of [connections] necessary to fully connect [circuits] (when connnecrting [connections] in order) *)
let rec connect_until_single_circuit
          (circuits : Point3DCircuits.t)
          (connections : connection list)
  =
  match connections with
  | ((a, b, _) as conn) :: tail ->
    let merged = Point3DCircuits.merge_circuits circuits a b in
    if
      Point3DCircuits.cardinal merged
      = Point3DCircuits.cardinal_circuit merged a (* merge resulted in one big circuit *)
    then conn
    else connect_until_single_circuit merged tail
  | _ -> failwith "Unable to add enough connections"
;;

let solve_part_one n lines =
  let points = List.map Point3D.parse lines |> Point3DSet.of_list in
  let graph = Point3DCircuits.init points in
  let connected = connect_n graph n (all_connections_ascending points) in
  Point3DCircuits.circuits connected
  |> List.map Point3DSet.cardinal
  |> List.sort (fun a b -> -Int.compare a b)
  |> List.take 3
  |> List.fold_left Int.mul 1
;;

let solve_part_two lines =
  let points = List.map Point3D.parse lines |> Point3DSet.of_list in
  let graph = Point3DCircuits.init points in
  let a, b, _ = connect_until_single_circuit graph (all_connections_ascending points) in
  a.x * b.x
;;

let example_input =
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
;;

let%expect_test "Solution Part One" =
  let solution = solve_part_one 1000 @@ Shared.read_input_lines "inputs/08.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 81536 |}]
;;

let%expect_test "Example Part One" =
  let solution = solve_part_one 10 example_input in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 40 |}]
;;

let%expect_test "Solution Part Two" =
  let solution = solve_part_two @@ Shared.read_input_lines "inputs/08.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 7017750530 |}]
;;

let%expect_test "Example Part Two" =
  let solution = solve_part_two example_input in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 25272 |}]
;;
