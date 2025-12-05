module Range : sig
  type t

  val within : t -> int64 -> bool
  val parse : string -> t
  val merge : t list -> t list
  val span : t -> Bigint.t
end = struct
  type t = int64 * int64

  let span ((low, high) : t) : Bigint.t =
    Bigint.(of_int64_exn high - of_int64_exn low + one)
  ;;

  let within ((low, high) : t) i = low <= i && i <= high

  let parse line : t =
    match String.split_on_char '-' line with
    | [ low; high ] -> Int64.of_string low, Int64.of_string high
    | _ -> failwith (Printf.sprintf "Invalid range string '%s'" line)
  ;;

  let compare (la, ha) (lb, hb) =
    let low = Int64.compare la lb in
    if low != 0 then low else Int64.compare ha hb
  ;;

  let merge (ranges : t list) : t list =
    let rec aux acc = function
      | [] -> List.rev acc
      | [ single ] -> List.rev (single :: acc)
      | ((la, ha) as a) :: ((lb, hb) as b) :: tail ->
        if ha < lb
        then aux (a :: acc) (b :: tail)
        else aux acc ((la, Int64.max ha hb) :: tail)
    in
    aux [] (List.sort compare ranges)
  ;;
end

let parse_input lines =
  let rec aux ranges = function
    | "" :: ingredients -> List.rev ranges, List.map Int64.of_string ingredients
    | range :: tail -> aux (Range.parse range :: ranges) tail
    | _ -> failwith "Missing ingredients"
  in
  aux [] lines
;;

let solve_part_one _lines =
  let ranges, ingredients = parse_input _lines in
  List.filter (fun i -> List.exists (fun r -> Range.within r i) ranges) ingredients
  |> List.length
;;

let solve_part_two _lines =
  let ranges, _ = parse_input _lines in
  let merged_ranges = Range.merge ranges in
  Shared.Reduction.AddBigint.reduce_list (List.map Range.span merged_ranges)
;;

let%expect_test "Solution Part One" =
  print_endline
    (Printf.sprintf "%d" (solve_part_one (Shared.read_input_lines "inputs/05.txt")));
  [%expect {| 511 |}]
;;

let%expect_test "Example Part One" =
  print_endline
    (Printf.sprintf
       "%d"
       (solve_part_one
          [ "3-5"; "10-14"; "16-20"; "12-18"; ""; "1"; "5"; "8"; "11"; "17"; "32" ]));
  [%expect {| 3 |}]
;;

let%expect_test "Solution Part Two" =
  print_endline
    (Printf.sprintf
       "%s"
       (solve_part_two (Shared.read_input_lines "inputs/05.txt") |> Bigint.to_string));
  [%expect {| 350939902751909 |}]
;;

let%expect_test "Example Part One" =
  print_endline
    (Printf.sprintf
       "%s"
       (solve_part_two
          [ "3-5"; "10-14"; "16-20"; "12-18"; ""; "1"; "5"; "8"; "11"; "17"; "32" ]
        |> Bigint.to_string));
  [%expect {| 14 |}]
;;
