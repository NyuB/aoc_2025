let youpi suffix = Printf.sprintf "Youpi %s" suffix

let%expect_test "Youpi" =
  Shared.read_input_lines "inputs/01.txt" |> List.map youpi |> List.iter print_endline;
  [%expect
    {|
    Youpi Kai
    Youpi Yeah
    Youpi Zou
    |}]
;;
