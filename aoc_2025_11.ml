(** Directed graph *)
module Graph = struct
  type node = string

  module NodeMap = Map.Make (String)
  module NodeSet = Set.Make (String)

  type t = NodeSet.t NodeMap.t

  let add_node (t : t) node : t =
    if NodeMap.mem node t then t else NodeMap.add node NodeSet.empty t
  ;;

  let add_connection (t : t) origin destination =
    NodeMap.update
      origin
      (function
        | None -> Some (NodeSet.singleton destination)
        | Some destinations -> Some (NodeSet.add destination destinations))
      (add_node t destination)
  ;;

  let set_connections (t : t) origin destinations =
    NodeSet.fold (fun destination t -> add_connection t origin destination) destinations t
  ;;

  let neighbours t origin = NodeMap.find origin t

  module MemoKey = struct
    type t = string * string

    let hash (a, b) = String.hash (Printf.sprintf "%s|%s" a b)
    let equal (a, b) (c, d) = String.equal a c && String.equal b d
  end

  module Memo = Hashtbl.Make (MemoKey)

  (** [count_paths t origin destination] is the number of different paths from [origin] to [destination] 
  Assumes an acyclic agraph, so does not track visited node to avoid infinite recursion *)
  let count_paths t origin destination =
    let rec aux (memo : int Memo.t) origin =
      let memo_key = origin, destination in
      if Memo.mem memo memo_key
      then Memo.find memo memo_key
      else (
        let res =
          if String.equal origin destination
          then 1
          else
            NodeSet.fold (fun node total -> total + aux memo node) (neighbours t origin) 0
        in
        Memo.add memo memo_key res;
        res)
    in
    aux (Memo.create 16) origin
  ;;

  let parse_connection conn =
    match String.split_on_char ':' conn with
    | [ origin; destinations ] ->
      origin, String.split_on_char ' ' (String.trim destinations) |> NodeSet.of_list
    | _ -> failwith (Printf.sprintf "Invalid connection line %s" conn)
  ;;

  let parse lines : t =
    List.fold_left
      (fun t line ->
         let origin, destinations = parse_connection line in
         set_connections t origin destinations)
      NodeMap.empty
      lines
  ;;
end

let solve_part_one lines =
  let graph = Graph.parse lines in
  Graph.count_paths graph "you" "out"
;;

let solve_part_two lines =
  let graph = Graph.parse lines in
  let count = Graph.count_paths graph in
  let svr_dac_fft_out = count "svr" "dac" * count "dac" "fft" * count "fft" "out"
  and svr_fft_dac_out = count "svr" "fft" * count "fft" "dac" * count "dac" "out" in
  (* Since there must be no cycle (otherwise there would be an infinite number of paths between nodes),
  only one of these two paths will be non-zero *)
  assert (svr_dac_fft_out = 0 || svr_fft_dac_out = 0);
  svr_dac_fft_out + svr_fft_dac_out
;;

let example_input_one =
  [ "aaa: you hhh"
  ; "you: bbb ccc"
  ; "bbb: ddd eee"
  ; "ccc: ddd eee fff"
  ; "ddd: ggg"
  ; "eee: out"
  ; "fff: out"
  ; "ggg: out"
  ; "hhh: ccc fff iii"
  ; "iii: out"
  ]
;;

let%expect_test "Solution Part One" =
  let solution = solve_part_one @@ Shared.read_input_lines "inputs/11.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 634 |}]
;;

let%expect_test "Example Part One" =
  let solution = solve_part_one example_input_one in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 5 |}]
;;

let example_input_two =
  [ "svr: aaa bbb"
  ; "aaa: fft"
  ; "fft: ccc"
  ; "bbb: tty"
  ; "tty: ccc"
  ; "ccc: ddd eee"
  ; "ddd: hub"
  ; "hub: fff"
  ; "eee: dac"
  ; "dac: fff"
  ; "fff: ggg hhh"
  ; "ggg: out"
  ; "hhh: out"
  ]
;;

let%expect_test "Solution Part Two" =
  let solution = solve_part_two @@ Shared.read_input_lines "inputs/11.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 377452269415704 |}]
;;

let%expect_test "Example Part Two" =
  let solution = solve_part_two example_input_two in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 2 |}]
;;

let%expect_test "Graph parsing" =
  let graph = Graph.parse example_input_one in
  Graph.NodeSet.iter print_endline (Graph.neighbours graph "you");
  [%expect
    {|
    bbb
    ccc
    |}]
;;
