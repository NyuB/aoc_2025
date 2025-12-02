let is_invalid_id id =
  let l = String.length id in
  if l mod 2 != 0
  then false
  else String.equal (String.sub id 0 (l / 2)) (String.sub id (l / 2) (l / 2))
;;

let rec is_repetition_of ~pattern id =
  let l = String.length id
  and lp = String.length pattern in
  if l == 0
  then true
  else if l mod lp != 0
  then false
  else if String.starts_with ~prefix:pattern id
  then is_repetition_of ~pattern (String.sub id lp (l - lp))
  else false
;;

let is_invalid_id_two id =
  let l = String.length id in
  let rec aux lp =
    if lp > l / 2
    then false
    else (
      let pattern = String.sub id 0 lp in
      if is_repetition_of ~pattern id then true else aux (lp + 1))
  in
  aux 1
;;

let parse_id_pair ids =
  match String.split_on_char '-' ids with
  | [ left; right ] -> Int64.of_string left, Int64.of_string right
  | _ -> failwith "Invalid id pair"
;;

let count_invalid_ids is_invalid (left, right) =
  let rec aux total current =
    if current > right
    then total
    else (
      let next_total =
        if is_invalid (Int64.to_string current) then Int64.add total current else total
      in
      aux next_total (Int64.add Int64.one current))
  in
  aux 0L left
;;

let solve_part_one line =
  String.split_on_char ',' line
  |> List.map parse_id_pair
  |> List.map (count_invalid_ids is_invalid_id)
  |> List.fold_left Int64.add 0L
;;

let solve_part_two line =
  String.split_on_char ',' line
  |> List.map parse_id_pair
  |> List.map (count_invalid_ids is_invalid_id_two)
  |> List.fold_left Int64.add 0L
;;

let example_input =
  String.concat
    ""
    [ "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,"
    ; "1698522-1698528,446443-446449,38593856-38593862,565653-565659,"
    ; "824824821-824824827,2121212118-2121212124"
    ]
;;

let%expect_test "Solution Part One" =
  print_endline
    (Printf.sprintf "%Ld" (solve_part_one (Shared.read_input_one_line "inputs/02.txt")));
  [%expect {| 54234399924 |}]
;;

let%expect_test "Example Part One" =
  print_endline (Printf.sprintf "%Ld" (solve_part_one example_input));
  [%expect {| 1227775554 |}]
;;

let%expect_test "Solution Part Two" =
  print_endline
    (Printf.sprintf "%Ld" (solve_part_two (Shared.read_input_one_line "inputs/02.txt")));
  [%expect {| 70187097315 |}]
;;

let%expect_test "Example Part Two" =
  print_endline (Printf.sprintf "%Ld" (solve_part_two example_input));
  [%expect {| 4174379265 |}]
;;
