let arithmetic_mod a b =
  let rem = a mod b in
  if rem >= 0 then rem else rem + b
;;

let rotate dial ~by = arithmetic_mod (dial + by) 100

type dial =
  { current : int
  ; zeros : int
  }

let rotate_left { current; zeros } ~by =
  let number_of_full_turns = by / 100
  and by = by mod 100 in
  let rotated = rotate current ~by:(-by) in
  let reached_zero =
    if (current != 0 && rotated > current) || rotated = 0 then zeros + 1 else zeros
  in
  { current = rotated; zeros = reached_zero + number_of_full_turns }
;;

let rotate_right { current; zeros } ~by =
  let number_of_full_turns = by / 100
  and by = by mod 100 in
  let rotated = rotate current ~by in
  let reached_zero = if rotated < current then zeros + 1 else zeros in
  { current = rotated; zeros = reached_zero + number_of_full_turns }
;;

let parse_dial line =
  let rotation = String.get line 0
  and by = String.sub line 1 (String.length line - 1) |> int_of_string in
  match rotation with
  | 'L' -> rotate_left ~by
  | 'R' -> rotate_right ~by
  | c -> failwith (Printf.sprintf "Invalid rotation %c" c)
;;

let solve_part_one lines =
  List.map parse_dial lines
  |> List.fold_left
       (fun (dial, count_zeros) rotation ->
          let ({ current = rotated; _ } as next_dial) = rotation dial in
          let zeros = if rotated = 0 then count_zeros + 1 else count_zeros in
          next_dial, zeros)
       ({ current = 50; zeros = 0 }, 0)
  |> snd
;;

let solve_part_two lines =
  let { zeros; _ } =
    List.map parse_dial lines
    |> List.fold_left (fun dial rotation -> rotation dial) { current = 50; zeros = 0 }
  in
  zeros
;;

let%expect_test "rotate" =
  print_endline (Printf.sprintf "%d" (rotate 0 ~by:1));
  print_endline (Printf.sprintf "%d" (rotate 0 ~by:(-1)));
  [%expect
    {|
    1
    99
    |}]
;;

let%expect_test "Solve part 1" =
  let lines = Shared.read_input_lines "inputs/01.txt" in
  print_endline (Printf.sprintf "%d" (solve_part_one lines));
  [%expect {| 997 |}]
;;

let%expect_test "Example part 1" =
  let lines = [ "L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82" ] in
  print_endline (Printf.sprintf "%d" (solve_part_one lines));
  [%expect {| 3 |}]
;;

let%expect_test "Solve part 2" =
  let lines = Shared.read_input_lines "inputs/01.txt" in
  print_endline (Printf.sprintf "%d" (solve_part_two lines));
  [%expect {| 5978 |}]
;;

let%expect_test "Example part 2" =
  let lines = [ "L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82" ] in
  print_endline (Printf.sprintf "%d" (solve_part_two lines));
  [%expect {| 6 |}]
;;
