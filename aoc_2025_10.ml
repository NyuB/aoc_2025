module StringMap = Map.Make (String)

type presses = int StringMap.t
type leds = string

module Item = struct
  type t =
    { leds : leds
    ; presses : int
    }

  let compare a b =
    let by_presses = Int.compare a.presses b.presses in
    if by_presses != 0 then by_presses else String.compare a.leds b.leds
  ;;

  let node_is t target = String.equal t.leds target

  let already_better_sequence item (presses : presses) =
    match StringMap.find_opt item.leds presses with
    | None -> false
    | Some d -> d <= item.presses
  ;;

  let update_presses item (presses : presses) =
    StringMap.add item.leds item.presses presses
  ;;
end

module Q = Pqueue.MakeMin (Item)

let rec shortest_flip_sequence
          (neighbours : leds -> leds list)
          (target : leds)
          (q : Q.t)
          (presses : presses)
  : int
  =
  let loop = shortest_flip_sequence neighbours target q in
  let item = Q.pop_min q |> Option.get in
  if Item.node_is item target
  then item.presses
  else if Item.already_better_sequence item presses
  then loop presses
  else (
    let next_presses = Item.update_presses item presses in
    let adjacents =
      neighbours item.leds
      |> List.map (fun n -> Item.{ leds = n; presses = item.presses + 1 })
    in
    Q.add_iter q List.iter adjacents;
    loop next_presses)
;;

module Counters = struct
  type t = int array

  let is_zero (t : t) = Array.for_all (( = ) 0) t
  let is_negative (t : t) = Array.exists (( > ) 0) t
  let total (t : t) = Shared.Reduction.AddInt.reduce_array t

  let compare a b =
    Array.combine a b
    |> Array.fold_left (fun cmp (a, b) -> if cmp != 0 then cmp else Int.compare a b) 0
  ;;
end

module Button = struct
  type t = int array

  let flip = function
    | '#' -> '.'
    | '.' -> '#'
    | c -> failwith (Printf.sprintf "Invalid state character %c" c)
  ;;

  let apply_leds button leds =
    let res = Bytes.init (String.length leds) (String.get leds) in
    Array.iter (fun i -> Bytes.set res i (flip (Bytes.get res i))) button;
    Bytes.to_string res
  ;;

  let apply_counters (button : t) (counters : Counters.t) =
    let res = Array.copy counters in
    Array.iter (fun c -> res.(c) <- res.(c) - 1) button;
    res
  ;;
end

module Solver = struct
  module Item = struct
    type t =
      { counters : Counters.t
      ; total : int
      ; total_press : int
      ; available_buttons : Button.t list
      }

    let starter counters (buttons : Button.t list) : t =
      let available_buttons =
        List.sort (fun a b -> -Int.compare (Array.length a) (Array.length b)) buttons
      in
      { counters; available_buttons; total = Counters.total counters; total_press = 0 }
    ;;

    let max_press_available { available_buttons; _ } =
      List.fold_left (fun m b -> max m (Array.length b)) Int.min_int available_buttons
    ;;

    let press t =
      let counters = Button.apply_counters (List.hd t.available_buttons) t.counters in
      if Counters.is_negative counters
      then None
      else
        Some
          { counters
          ; total = Counters.total counters
          ; total_press = t.total_press + 1
          ; available_buttons = t.available_buttons
          }
    ;;

    let next_button t =
      if List.length t.available_buttons < 2
      then None
      else Some { t with available_buttons = List.tl t.available_buttons }
    ;;

    let heuristic t = t.total_press + (t.total / max_press_available t)
    let compare a b = Int.compare (heuristic a) (heuristic b)
  end

  module Q = Pqueue.MakeMin (Item)

  let rec solve q =
    match Q.pop_min q with
    | None -> failwith "No solution"
    | Some { total = 0; total_press; _ } -> total_press
    | Some item ->
      let next = [ Item.next_button item; Item.press item ] |> List.filter_map Fun.id in
      List.iter (Q.add q) next;
      solve q
  ;;

  let solve buttons counters =
    let q = Q.of_list [ Item.starter counters buttons ] in
    solve q
  ;;
end

module Solver2 = struct
  module CounterMap = Map.Make (Counters)

  type presses = int CounterMap.t

  module Item = struct
    type t =
      { counters : Counters.t
      ; presses : int
      ; total : int
      }

    let already_better_sequence item (presses : presses) =
      match CounterMap.find_opt item.counters presses with
      | None -> false
      | Some d -> d <= item.presses
    ;;

    let update_presses item (presses : presses) =
      CounterMap.add item.counters item.presses presses
    ;;

    let compare a b =
      let best_button = Array.length a.counters in
      Int.compare
        (a.presses + (a.total / best_button))
        (b.presses + (b.total / best_button))
    ;;
  end

  module Q = Pqueue.MakeMin (Item)

  let rec shortest_inc_sequence
            (neighbours : Counters.t -> Counters.t list)
            (q : Q.t)
            (presses : presses)
    : int
    =
    let loop = shortest_inc_sequence neighbours q in
    let item = Q.pop_min q |> Option.get in
    if Counters.is_zero item.counters
    then item.presses
    else if Item.already_better_sequence item presses
    then loop presses
    else (
      let next_presses = Item.update_presses item presses in
      let adjacents =
        neighbours item.counters
        |> List.filter (fun i -> not @@ Counters.is_negative i)
        |> List.map (fun n ->
          Item.{ counters = n; presses = item.presses + 1; total = Counters.total n })
      in
      Q.add_iter q List.iter adjacents;
      loop next_presses)
  ;;

  let neighbours_counters (buttons : Button.t list) counters : Counters.t list =
    List.map (fun b -> Button.apply_counters b counters) buttons
  ;;

  let solve counters buttons =
    let q =
      Q.of_list [ Item.{ counters; presses = 0; total = Counters.total counters } ]
    in
    let presses = CounterMap.empty in
    shortest_inc_sequence (neighbours_counters buttons) q presses
  ;;
end

module Solver3 = struct
  let mask (buttons : Button.t list) (counters : Counters.t) =
    let rec aux counters = function
      | [] -> counters
      | button :: tail ->
        Array.iter (fun i -> counters.(i) <- 0) button;
        aux counters tail
    in
    aux (Array.copy counters) buttons
  ;;

  let can_solve buttons counters =
    (not (Counters.is_negative counters)) && Counters.is_zero (mask buttons counters)
  ;;

  let rec solve (best : int) (current : int) buttons counters =
    if Counters.is_zero counters
    then min best current
    else if (not @@ can_solve buttons counters) || current >= best
    then best
    else (
      match buttons with
      | [] -> best
      | button :: tail ->
        let best_applied =
          solve best (current + 1) buttons (Button.apply_counters button counters)
        in
        min best_applied (solve best_applied current tail counters))
  ;;

  let solve buttons counters =
    solve
      Int.max_int
      0
      (List.sort (fun a b -> -Int.compare (Array.length a) (Array.length b)) buttons)
      counters
  ;;
end

let neighbours (buttons : Button.t list) state =
  List.map (fun b -> Button.apply_leds b state) buttons
;;

let solve_one target buttons =
  let start = String.init (String.length target) (Fun.const '.') in
  let q = Q.of_list [ Item.{ leds = start; presses = 0 } ] in
  let presses = StringMap.empty in
  shortest_flip_sequence (neighbours buttons) target q presses
;;

let without_bracket s = String.sub s 1 (String.length s - 2)
let parse_flip_target target = without_bracket target

let parse_button button : Button.t =
  String.split_on_char ',' (without_bracket button)
  |> List.map int_of_string
  |> Array.of_list
;;

let parse_joltages joltages =
  String.split_on_char ',' (without_bracket joltages)
  |> List.map int_of_string
  |> Array.of_list
;;

let parse_buttons_and_joltages buttons_and_joltages =
  match List.rev buttons_and_joltages with
  | [] -> failwith "Needs at least joltages"
  | joltages :: buttons -> List.map parse_button buttons, parse_joltages joltages
;;

let parse_one line =
  match String.split_on_char ' ' line with
  | target :: buttons_and_joltages ->
    let buttons, _ = parse_buttons_and_joltages buttons_and_joltages in
    parse_flip_target target, buttons
  | _ -> failwith (Printf.sprintf "Invalid line '%s'" line)
;;

let parse_two line : Button.t list * Counters.t =
  match String.split_on_char ' ' line with
  | _ :: buttons_and_joltages ->
    let buttons, joltages = parse_buttons_and_joltages buttons_and_joltages in
    buttons, joltages
  | _ -> failwith (Printf.sprintf "Invalid line '%s'" line)
;;

let solve_part_one lines =
  List.map parse_one lines
  |> List.map (fun (start, buttons) -> solve_one start buttons)
  |> Shared.Reduction.AddInt.reduce_list
;;

let solve_part_two lines =
  List.map parse_two lines
  |> List.map (fun (buttons, joltages) -> Solver3.solve buttons joltages)
  |> Shared.Reduction.AddInt.reduce_list
;;

let example_input =
  [ "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
  ; "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
  ; "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
  ]
;;

let%expect_test "Solution Part One" =
  let solution = solve_part_one @@ Shared.read_input_lines "inputs/10.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 452 |}]
;;

let%expect_test "Example Part One" =
  let solution = solve_part_one example_input in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 7 |}]
;;

let%expect_test "Solution Part Two" =
  let solution = solve_part_two @@ Shared.read_input_lines "inputs/10.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 0 |}]
;;

let%expect_test "Example Part Two" =
  let solution = solve_part_two example_input in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 33 |}]
;;
