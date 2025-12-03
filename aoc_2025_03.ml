let digits line =
  String.to_seq line |> Seq.map (Fun.compose int_of_string Char.escaped) |> Array.of_seq
;;

let rec find_max_item_index best best_index ~from arr =
  if from >= Array.length arr
  then best_index
  else (
    let best, best_index =
      if arr.(from) > best then arr.(from), from else best, best_index
    in
    find_max_item_index best best_index ~from:(from + 1) arr)
;;

let find_max_item_index ~from arr = find_max_item_index (-1) (-1) ~from arr
let without_last arr = Array.sub arr 0 (Array.length arr - 1)
let since index arr = Array.sub arr index (Array.length arr - index)

let rec best_joltage best arr n =
  if n == 0
  then best
  else (
    let available = Array.sub arr 0 (Array.length arr - n + 1) in
    let digit_index = find_max_item_index ~from:0 available in
    best_joltage
      ((best * 10) + available.(digit_index))
      (since (digit_index + 1) arr)
      (n - 1))
;;

let best_joltage arr n = best_joltage 0 arr n

let solve_part_one lines : int =
  let digits = List.map digits lines in
  List.fold_left (fun total digits -> total + best_joltage digits 2) 0 digits
;;

let solve_part_two lines : int =
  let digits = List.map digits lines in
  List.fold_left (fun total digits -> total + best_joltage digits 12) 0 digits
;;

let%expect_test "Solution Part One" =
  print_endline
    (Printf.sprintf "%d" (solve_part_one (Shared.read_input_lines "inputs/03.txt")));
  [%expect {| 17332 |}]
;;

let%expect_test "Solution Part Two" =
  print_endline
    (Printf.sprintf "%d" (solve_part_two (Shared.read_input_lines "inputs/03.txt")));
  [%expect {| 172516781546707 |}]
;;

let%expect_test "Example Part One" =
  print_endline
    (Printf.sprintf
       "%d"
       (solve_part_one
          [ "987654321111111"; "811111111111119"; "234234234234278"; "818181911112111" ]));
  [%expect {| 357 |}]
;;

let%expect_test "Example Part One" =
  print_endline
    (Printf.sprintf
       "%d"
       (solve_part_two
          [ "987654321111111"; "811111111111119"; "234234234234278"; "818181911112111" ]));
  [%expect {| 3121910778619 |}]
;;
