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

module Button = struct
  type t = int array

  let flip = function
    | '#' -> '.'
    | '.' -> '#'
    | c -> failwith (Printf.sprintf "Invalid state character %c" c)
  ;;

  let apply button leds =
    let res = Bytes.init (String.length leds) (String.get leds) in
    Array.iter (fun i -> Bytes.set res i (flip (Bytes.get res i))) button;
    Bytes.to_string res
  ;;
end

let neighbours (buttons : Button.t list) state =
  List.map (fun b -> Button.apply b state) buttons
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

let parse_buttons_and_joltages buttons_and_joltages =
  match List.rev buttons_and_joltages with
  | [] -> failwith "Needs at least joltages"
  | _joltages :: buttons -> List.map parse_button buttons
;;

let parse_one line =
  match String.split_on_char ' ' line with
  | target :: buttons_and_joltages ->
    parse_flip_target target, parse_buttons_and_joltages buttons_and_joltages
  | _ -> failwith (Printf.sprintf "Invalid line '%s'" line)
;;

let solve_part_one lines =
  List.map parse_one lines
  |> List.map (fun (start, buttons) -> solve_one start buttons)
  |> Shared.Reduction.AddInt.reduce_list
;;

let solve_part_two _ = 0

let%expect_test "Solution Part One" =
  let solution = solve_part_one @@ Shared.read_input_lines "inputs/10.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 452 |}]
;;

let%expect_test "Example Part One" =
  let solution =
    solve_part_one
      [ "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
      ; "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
      ; "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
      ]
  in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 7 |}]
;;

let%expect_test "Solution Part Two" =
  let solution = solve_part_two @@ Shared.read_input_lines "inputs/10.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 0 |}]
;;

let%expect_test "Example Part Two" =
  let solution = solve_part_two [ (* Insert example input here *) ] in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 0 |}]
;;
