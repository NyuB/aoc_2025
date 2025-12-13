type part =
  | One
  | Two

let part_of_string s =
  match String.lowercase_ascii s with
  | "one" -> Some One
  | "two" -> Some Two
  | _ -> None
;;

module type MultiLineIntResult = sig
  val solve_part_one : string list -> int
  val solve_part_two : string list -> int
end

module IntMap = Map.Make (Int)

let usual_day_map : (module MultiLineIntResult) IntMap.t =
  IntMap.of_list
    [ 1, (module Aoc_2025_01 : MultiLineIntResult)
    ; (* day 02 omitted *)
      3, (module Aoc_2025_03 : MultiLineIntResult)
    ; 4, (module Aoc_2025_04 : MultiLineIntResult)
    ; 6, (module Aoc_2025_06 : MultiLineIntResult)
    ; 7, (module Aoc_2025_07 : MultiLineIntResult)
    ; ( 8
      , (module struct
          let solve_part_one = Aoc_2025_08.solve_part_one 1000
          let solve_part_two = Aoc_2025_08.solve_part_two
        end : MultiLineIntResult) )
    ; 9, (module Aoc_2025_09 : MultiLineIntResult)
    ; 10, (module Aoc_2025_10 : MultiLineIntResult)
    ; 11, (module Aoc_2025_11 : MultiLineIntResult)
    ; 12, (module Aoc_2025_12 : MultiLineIntResult)
    ]
;;

let solve day part input_file =
  if not @@ Sys.file_exists input_file
  then prerr_endline (Printf.sprintf "Input file '%s' not found" input_file)
  else (
    match IntMap.find_opt day usual_day_map with
    | Some (module M) ->
      let res =
        if part = One
        then M.solve_part_one (Shared.read_input_lines input_file)
        else M.solve_part_two (Shared.read_input_lines input_file)
      in
      print_endline (Printf.sprintf "%d" res)
    | None ->
      (match day, part with
       | 2, One ->
         let res = Aoc_2025_02.solve_part_one (Shared.read_input_one_line input_file) in
         print_endline (Printf.sprintf "%Ld" res)
       | 2, Two ->
         let res = Aoc_2025_02.solve_part_two (Shared.read_input_one_line input_file) in
         print_endline (Printf.sprintf "%Ld" res)
       | 5, One ->
         let res = Aoc_2025_05.solve_part_one (Shared.read_input_lines input_file) in
         print_endline (Printf.sprintf "%d" res)
       | 5, Two ->
         let res = Aoc_2025_05.solve_part_two (Shared.read_input_lines input_file) in
         print_endline (Printf.sprintf "%s" (Bigint.to_string res))
       | d, _ ->
         prerr_endline
           (Printf.sprintf
              "Invalid day '%d'%s"
              d
              (if 0 < d && d <= 25 then " (NB: in 2025, AOC only had 12 days)" else ""));
         exit 1))
;;

let () =
  match Sys.argv with
  | [| _; day; part; input_file |] ->
    (match part_of_string part with
     | Some part -> solve (int_of_string day) part input_file
     | None ->
       prerr_endline (Printf.sprintf "Invalid part '%s' (must be 'One' or 'Two')" part);
       exit 1)
  | _ ->
    prerr_endline (Printf.sprintf "Usage: %s day part input_file" Sys.argv.(0));
    exit 1
;;
