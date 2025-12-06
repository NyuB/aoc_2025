let () =
  let day = int_of_string Sys.argv.(1) in
  let output_file = Printf.sprintf "aoc_2025_%02d.ml" day in
  let input_file = Printf.sprintf "inputs/%02d.txt" day in
  if not @@ Sys.file_exists input_file
  then Out_channel.with_open_text input_file (fun oc -> Out_channel.flush oc);
  if not @@ Sys.file_exists output_file
  then
    Out_channel.with_open_text output_file (fun oc ->
      Printf.fprintf
        oc
        {z|
let solve_part_one _ = 0

let solve_part_two _ = 0

let%%expect_test "Solution Part One" =
    let solution = solve_part_one @@ Shared.read_input_lines "%s" in
    print_endline @@ Printf.sprintf "%%d" solution;
    [%%expect {| 0 |}]
;;
let%%expect_test "Example Part One" =
    let solution = solve_part_one [(* Insert example input here *)] in
    print_endline @@ Printf.sprintf "%%d" solution;
    [%%expect {| 0 |}]
;;

let%%expect_test "Solution Part Two" =
    let solution = solve_part_two @@ Shared.read_input_lines "%s" in
    print_endline @@ Printf.sprintf "%%d" solution;
    [%%expect {| 0 |}]
;;

let%%expect_test "Example Part Two" =
    let solution = solve_part_two [(* Insert example input here *)] in
    print_endline @@ Printf.sprintf "%%d" solution;
    [%%expect {| 0 |}]
;;
|z}
        input_file
        input_file)
;;
