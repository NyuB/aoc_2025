(* Helpers *)
let all_empty_heads lists = List.for_all (fun l -> List.hd l = ' ') lists
let all_empty lists = List.for_all (( = ) []) lists
let tails lists = List.map List.tl lists
let heads lists = List.map List.hd lists
let revs lists = List.map List.rev lists

let cons_heads lists list_list =
  if List.length lists != List.length list_list
  then
    failwith
      (Printf.sprintf "Invalid cons %d vs %d" (List.length lists) (List.length list_list));
  List.map2 (fun h l -> h :: l) (heads lists) list_list
;;

let one_for_each item list = List.map (Fun.const item) list
let list_of_string = Fun.compose List.of_seq String.to_seq

let split_on_spaces s =
  let l = String.length s in
  let rec count_non_spaces_from count i =
    if i >= l
    then count
    else if String.get s i != ' '
    then count_non_spaces_from (count + 1) (i + 1)
    else count
  in
  let count_non_spaces_from = count_non_spaces_from 0 in
  let rec aux acc i =
    if i >= l
    then List.rev acc
    else if String.get s i = ' '
    then aux acc (i + 1)
    else (
      let non_spaces = count_non_spaces_from i in
      aux (String.sub s i non_spaces :: acc) (i + non_spaces))
  in
  aux [] 0
;;

let rows_to_cols rows =
  let lists = List.map (fun _ -> []) (List.nth rows 0) in
  let rec aux acc = function
    | [] -> List.map List.rev acc
    | row :: tail -> aux (List.map2 (fun l item -> item :: l) acc row) tail
  in
  aux lists rows
;;

(* Helpers end *)

module Operation = struct
  type t =
    | Add
    | Mul

  let parse = function
    | "+" -> Add
    | "*" -> Mul
    | s -> failwith (Printf.sprintf "Invalid operator '%s'" s)
  ;;

  let apply t a b =
    match t with
    | Add -> a + b
    | Mul -> a * b
  ;;

  let neutral = function
    | Add -> 0
    | Mul -> 1
  ;;

  let apply_list t numbers =
    List.fold_left (fun total n -> apply t total n) (neutral t) numbers
  ;;
end

module OctopusProblem = struct
  type t = int list * Operation.t

  (** Expects [lines] to already be formatted as [[n1; n2; n3 ... operator]]*)
  let parse_columns lines : t =
    let rec aux numbers = function
      | [] -> failwith "Missing operator"
      | [ operator ] -> List.rev numbers, Operation.parse operator
      | number :: tail -> aux (int_of_string number :: numbers) tail
    in
    aux [] lines
  ;;

  let split_on_empty_column lists =
    let rec aux acc current lists =
      if List.is_empty lists then failwith "Invalid empty list";
      if all_empty lists
      then List.rev (if all_empty current then acc else revs current :: acc)
      else if all_empty_heads lists
      then aux (revs current :: acc) (one_for_each [] lists) (tails lists)
      else aux acc (cons_heads lists current) (tails lists)
    in
    aux [] (one_for_each [] lists) (List.map list_of_string lists)
    |> List.map (fun row -> List.map (fun cs -> List.to_seq cs |> String.of_seq) row)
  ;;

  let add_digit num = function
    | ' ' -> num
    | d -> (10 * num) + int_of_string (Char.escaped d)
  ;;

  let octo_numbers digit_lists =
    let rec aux acc = function
      | [] -> acc
      | digit_list :: tail -> aux (List.map2 add_digit acc digit_list) tail
    in
    let digit_lists = List.map list_of_string digit_lists in
    aux (one_for_each 0 (List.hd digit_lists)) digit_lists
  ;;

  (** Parses directly from the input format *)
  let parse_octo (lines : string list) : t list =
    let rec aux acc = function
      | [] -> failwith "Missing operators"
      | [ operators ] -> List.rev acc, split_on_spaces operators
      | numbers :: tail -> aux (numbers :: acc) tail
    in
    let numbers, operators = aux [] lines in
    let octo = split_on_empty_column numbers |> List.map octo_numbers in
    List.map2 (fun n o -> n, o) octo (List.map Operation.parse operators)
  ;;

  let solve ((numbers, operator) : t) : int = Operation.apply_list operator numbers
end

let solve_part_one lines =
  lines
  |> List.map split_on_spaces
  |> rows_to_cols
  |> List.map OctopusProblem.parse_columns
  |> List.map OctopusProblem.solve
  |> List.fold_left ( + ) 0
;;

let solve_part_two lines =
  lines
  |> OctopusProblem.parse_octo
  |> List.map OctopusProblem.solve
  |> List.fold_left ( + ) 0
;;

let%expect_test "Solution Part One" =
  let solution = solve_part_one @@ Shared.read_input_lines "inputs/06.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 4309240495780 |}]
;;

let%expect_test "Example Part One" =
  let solution =
    solve_part_one
      [ "123 328  51 64 "; " 45 64  387 23 "; "  6 98  215 314"; "*   +   *   + " ]
  in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 4277556 |}]
;;

let%expect_test "Solution Part Two" =
  let solution = solve_part_two @@ Shared.read_input_lines "inputs/06.txt" in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 9170286552289 |}]
;;

let%expect_test "Example Part Two" =
  let solution =
    solve_part_two
      [ "123 328  51 64 "; " 45 64  387 23 "; "  6 98  215 314"; "*   +   *   + " ]
  in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 3263827 |}]
;;

let ( <| ) f g = Fun.compose f g
let print_list = print_endline <| (Printf.sprintf "[%s]" <| String.concat ";")

let print_list_list l =
  List.iter (print_endline <| Printf.sprintf "[ %s ]" <| String.concat "; ") l
;;

let%expect_test "Split on spaces" =
  print_list @@ split_on_spaces "1  2     4";
  [%expect {| [1;2;4] |}]
;;

let%expect_test "Rows to columns" =
  let l = [ [ "A"; "B"; "C" ]; [ "D"; "E"; "F" ] ] in
  print_list_list l;
  print_endline "---";
  print_list_list (rows_to_cols l);
  print_endline "---";
  print_list_list (rows_to_cols (rows_to_cols l));
  [%expect
    {|
    [ A; B; C ]
    [ D; E; F ]
    ---
    [ A; D ]
    [ B; E ]
    [ C; F ]
    ---
    [ A; B; C ]
    [ D; E; F ]
    |}]
;;

let%expect_test "Split on empty columns" =
  let l = [ "123 45"; " 67  8" ] in
  print_list_list (OctopusProblem.split_on_empty_column l);
  [%expect
    {|
    [ 123;  67 ]
    [ 45;  8 ]
    |}]
;;

let%expect_test "octo numbers" =
  let l = [ "123 45"; " 67  8" ] in
  OctopusProblem.split_on_empty_column l
  |> List.map OctopusProblem.octo_numbers
  |> List.map (List.map string_of_int)
  |> print_list_list;
  [%expect
    {|
    [ 1; 26; 37 ]
    [ 4; 58 ]
    |}]
;;
