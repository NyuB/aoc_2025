module StringMap = Map.Make (String)

type leds = string

let without_bracket s = String.sub s 1 (String.length s - 2)

module Button = struct
  type t = int iarray

  let flip = function
    | '#' -> '.'
    | '.' -> '#'
    | c -> failwith (Printf.sprintf "Invalid state character %c" c)
  ;;

  let apply_leds (button : t) leds =
    let res = Bytes.init (String.length leds) (String.get leds) in
    Iarray.iter (fun i -> Bytes.set res i (flip (Bytes.get res i))) button;
    Bytes.to_string res
  ;;

  let parse (s : string) : t =
    String.split_on_char ',' (without_bracket s)
    |> List.map int_of_string
    |> Iarray.of_list
  ;;
end

(** Solves part one exploring all states with a BFS algorithm *)
module Solver_One = struct
  type presses = int StringMap.t

  module Item = struct
    type t =
      { leds : leds
      ; presses : int
      }

    let compare a b = Int.compare a.presses b.presses
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

  let rec shortest_flip_sequence
            (neighbours : leds -> leds list)
            (target : leds)
            (current : Item.t list)
            (presses : presses)
    : int
    =
    if current = []
    then failwith "Unsolvable"
    else
      let exception Return of int in
      try
        let next, next_presses =
          List.fold_left
            (fun (next, next_presses) item ->
               if Item.node_is item target
               then raise (Return item.presses)
               else if Item.already_better_sequence item next_presses
               then next, next_presses
               else (
                 let next_presses = Item.update_presses item next_presses in
                 let adjacents =
                   neighbours item.leds
                   |> List.map (fun n -> Item.{ leds = n; presses = item.presses + 1 })
                 in
                 List.rev_append adjacents next, next_presses))
            ([], presses)
            current
        in
        shortest_flip_sequence neighbours target next next_presses
      with
      | Return n -> n
  ;;

  let neighbours (buttons : Button.t list) state =
    List.map (fun b -> Button.apply_leds b state) buttons
  ;;

  let solve target buttons =
    let start = String.init (String.length target) (Fun.const '.') in
    let start = [ Item.{ leds = start; presses = 0 } ] in
    let presses = StringMap.empty in
    shortest_flip_sequence (neighbours buttons) target start presses
  ;;
end

module Solver_Two = struct
  (** variables' coefficients and sum *)
  type equation = int iarray * int

  type t =
    { nb_variables : int
    ; equations : equation iarray
    }

  let equations_left_side_of_buttons nb_equations nb_variables buttons =
    let equations = Array.init nb_equations (fun _ -> Array.make nb_variables 0) in
    List.iteri (fun bi b -> Iarray.iter (fun c -> equations.(c).(bi) <- 1) b) buttons;
    Array.map Iarray.of_array equations |> Iarray.of_array
  ;;

  let of_buttons_and_joltages (buttons : Button.t list) joltages =
    let nb_variables = List.length buttons in
    let nb_equations = Iarray.length joltages in
    { nb_variables
    ; equations =
        Iarray.combine
          (equations_left_side_of_buttons nb_equations nb_variables buttons)
          joltages
    }
  ;;

  module MathProg = struct
    let var_name_of_var_id i =
      [| "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"; "n"; "o"; "p" |].(
      i)
    ;;

    let var_declaration var_id =
      Printf.sprintf "var %s integer;" (var_name_of_var_id var_id)
    ;;

    let var_coeff_in_sum i coeff =
      if coeff < 2
      then var_name_of_var_id i
      else Printf.sprintf "%d * %s" coeff (var_name_of_var_id i)
    ;;

    let equation_constraint (equation_index : int) ((vars, eq) : equation) : string =
      let vars =
        Iarray.mapi (fun i coeff -> coeff, var_coeff_in_sum i coeff) vars
        |> Iarray.to_list
        |> List.filter_map (fun (coeff, v) -> if coeff = 0 then None else Some v)
      in
      let sum = String.concat " + " vars in
      Printf.sprintf "s.t. eq_%d: %s == %d;" equation_index sum eq
    ;;

    let positive_constraint (var_id : int) : string =
      let var_name = var_name_of_var_id var_id in
      Printf.sprintf "s.t. %s_positive: %s >= 0;" var_name var_name
    ;;

    let minimize nb_variables =
      let sum = List.init nb_variables var_name_of_var_id |> String.concat "+" in
      Printf.sprintf "minimize sum: %s;" sum
    ;;

    let program (t : t) : string =
      [ "# start of program"; "# minimizes the value: sum" ]
      @ List.init t.nb_variables var_declaration
      @ [ minimize t.nb_variables ]
      @ (Iarray.mapi equation_constraint t.equations |> Iarray.to_list)
      @ List.init t.nb_variables positive_constraint
      @ [ "solve;"; "printf \"%d\\n\",sum;"; "# end of program"; "" ]
      |> String.concat "\n"
    ;;

    let read_until_last ic =
      let rec aux last =
        match In_channel.input_line ic with
        | None -> last
        | Some line -> aux (Some line)
      in
      aux None |> Option.get
    ;;

    let run_glpsol model_file output_file =
      let solver_exit_code =
        Sys.command
          (Printf.sprintf
             "glpsol --math %s --display %s > /dev/null"
             model_file
             output_file)
      in
      assert (solver_exit_code = 0);
      In_channel.with_open_bin output_file (fun ic ->
        let output = read_until_last ic in
        match int_of_string_opt output with
        | None -> failwith (Printf.sprintf "Last line of output was '%s'" output)
        | Some i -> i)
    ;;

    let solve t =
      let model_file = Filename.temp_file "model" ".gmpl" in
      let output_file = Filename.temp_file (Filename.basename model_file) ".out" in
      Out_channel.with_open_bin model_file (fun oc ->
        Out_channel.output_string oc (program t));
      run_glpsol model_file output_file
    ;;
  end

  (** A simplex implementation handling only equality constraints and 1/0 ponderations *)
  module Simplex = struct
    type optimization =
      | Maximize
      | Minimize

    let cost_coeff = function
      | Maximize -> -1
      | Minimize -> 1
    ;;

    let ( .%() ) arr i = Iarray.get arr i

    type tableau = int array array
    type problem = t (* Avoid shadowing *)

    type t =
      { rows : int
      ; columns : int
      ; tableau : tableau
      }

    let nb_equations t = t.rows - 1
    let nb_artificals t = nb_equations t
    let nb_variables t = t.columns - 2 - nb_artificals t

    let rec is_basic t one i j =
      if i >= t.rows
      then one
      else if t.tableau.(i).(j) = 1
      then if Option.is_some one then None else is_basic t (Some i) (i + 1) j
      else if t.tableau.(i).(j) != 0
      then None
      else is_basic t one (i + 1) j
    ;;

    let is_basic t j = is_basic t None 0 j

    let rec find_negativest_column t j best =
      if j >= t.columns
      then best
      else (
        let loop = find_negativest_column t (j + 1) in
        let value = t.tableau.(t.rows - 1).(j) in
        if value < 0
        then (
          match best with
          | None -> loop (Some (j, value))
          | Some (_, best_value) ->
            if best_value > value then loop (Some (j, value)) else loop best)
        else loop best)
    ;;

    let find_negativest_column t = find_negativest_column t 0 None |> Option.map fst
    let rhs t i = t.tableau.(i).(t.columns - 1)

    let normalize_row t i j =
      let divider = t.tableau.(i).(j) in
      for j = 0 to t.columns - 1 do
        t.tableau.(i).(j) <- t.tableau.(i).(j) / divider
      done
    ;;

    let substract_row t ~by i =
      let by_i, by_j = by in
      if i = by_i
      then ()
      else (
        let factor = t.tableau.(i).(by_j) in
        for j = 0 to t.columns - 1 do
          t.tableau.(i).(j) <- t.tableau.(i).(j) - (t.tableau.(by_i).(j) * factor)
        done)
    ;;

    let rec find_smallest_ratio t j i best =
      if i >= t.rows - 1
      then best
      else (
        let loop = find_smallest_ratio t j (i + 1) in
        let xi = t.tableau.(i).(j)
        and bi = rhs t i in
        if xi <= 0
        then loop best
        else (
          let ratio = bi / xi in
          match best with
          | None -> loop (Some (i, ratio))
          | Some (_, best_value) ->
            if ratio < best_value then loop (Some (i, ratio)) else loop best))
    ;;

    let find_smallest_ratio t j = find_smallest_ratio t j 0 None |> Option.map fst

    let find_pivot t =
      let j = find_negativest_column t |> Option.get in
      let i = find_smallest_ratio t j |> Option.get in
      i, j
    ;;

    let rec find_basic_artificial t j =
      if j >= t.columns - 2
      then None
      else (
        match is_basic t j with
        | None -> find_basic_artificial t (j + 1)
        | Some i -> Some (i, j))
    ;;

    let find_basic_artificial t = find_basic_artificial t (nb_variables t)

    let rec find_non_basic_variable t i j =
      if j >= nb_variables t
      then None
      else if t.tableau.(i).(j) = 0 || Option.is_some (is_basic t j)
      then find_non_basic_variable t i (j + 1)
      else Some j
    ;;

    let find_non_basic_variable t i = find_non_basic_variable t i 0

    let step_pivot t ((pivot_i, pivot_j) as pivot) =
      normalize_row t pivot_i pivot_j;
      for i = 0 to t.rows - 1 do
        substract_row t ~by:pivot i
      done
    ;;

    let should_eliminate_artificial t =
      match find_basic_artificial t with
      | Some (ai, _) -> Option.is_some (find_non_basic_variable t ai)
      | None -> false
    ;;

    let is_solved t =
      Option.is_none (find_negativest_column t) && (not @@ should_eliminate_artificial t)
    ;;

    let step_artificial t ai =
      match find_non_basic_variable t ai with
      | Some xj -> step_pivot t (ai, xj)
      | None -> ()
    ;;

    let step t =
      if Option.is_some (find_negativest_column t)
      then step_pivot t (find_pivot t)
      else (
        match find_basic_artificial t with
        | Some (ai, _) -> step_artificial t ai
        | None -> ())
    ;;

    let init problem optimization =
      let columns =
        problem.nb_variables
        + Iarray.length problem.equations (* one artificial variable per equation *)
        + 1 (* objective *)
        + 1 (* coefficient *)
      in
      let rows =
        Iarray.length problem.equations + 1
        (* objective row *)
      in
      let tableau =
        Array.init_matrix rows columns (fun i j ->
          if i = rows - 1
          then
            if j < problem.nb_variables
            then cost_coeff optimization
            else if j = columns - 2
            then 1
            else 0
          else if j = columns - 1
          then snd problem.equations.%(i)
          else if j = columns - 2
          then 0
          else if j >= problem.nb_variables
          then if i = j - problem.nb_variables then 1 else 0
          else (fst problem.equations.%(i)).%(j))
      in
      { rows; columns; tableau }
    ;;

    let rec repeat (i : int) (f : int -> unit) (nb : int) : unit =
      if nb <= 0 then () else f i;
      repeat (i + 1) f (nb - 1)
    ;;

    let repeat (f : int -> unit) (nb : int) : unit = repeat 0 f nb
    let var_name id = Printf.sprintf "x%d" (id + 1)
    let artificial_var_name id = Printf.sprintf "a%d" (id + 1)

    let format_header t j =
      if j < nb_variables t
      then Printf.sprintf " %s" (var_name j)
      else if j < t.columns - 2
      then Printf.sprintf " %s" (artificial_var_name (j - nb_variables t))
      else if j = t.columns - 2
      then "    z"
      else "  b"
    ;;

    let format_int t j n =
      let spacing = if j = t.columns - 2 then "  " else "" in
      Printf.sprintf "%s%3d" spacing n
    ;;

    let row_separator t = String.make ((t.columns * 4) + 2) '-'

    let print t =
      List.init t.columns (format_header t) |> String.concat "|" |> print_endline;
      print_endline @@ row_separator t;
      Array.iter
        (fun row ->
           Array.mapi (format_int t) row
           |> Array.to_list
           |> String.concat "|"
           |> print_endline;
           print_endline @@ row_separator t)
        t.tableau
    ;;

    let rec step_until_solved simplex =
      if is_solved simplex
      then simplex.tableau.(simplex.rows - 1).(simplex.columns - 1)
      else (
        step simplex;
        step_until_solved simplex)
    ;;

    let dual (problem : problem) =
      let nb_var = Iarray.length problem.equations in
      let nb_equations = problem.nb_variables in
      let equations =
        Iarray.init nb_equations (fun i ->
          Iarray.init nb_var (fun j -> (fst problem.equations.%(j)).%(i)), 1)
      in
      let result = init { equations; nb_variables = nb_var } Minimize in
      for j = 0 to nb_var - 1 do
        result.tableau.(result.rows - 1).(j) <- -snd problem.equations.%(j)
      done;
      result
    ;;

    let maximize problem = step_until_solved (init problem Maximize)
    let minimize problem = step_until_solved (dual problem)
  end
end

let parse_flip_target target = without_bracket target

let parse_joltages joltages =
  String.split_on_char ',' (without_bracket joltages)
  |> List.map int_of_string
  |> Iarray.of_list
;;

let parse_buttons_and_joltages buttons_and_joltages =
  match List.rev buttons_and_joltages with
  | [] -> failwith "Needs at least joltages"
  | joltages :: buttons ->
    List.map Button.parse buttons |> List.rev, parse_joltages joltages
;;

let parse_one line =
  match String.split_on_char ' ' line with
  | target :: buttons_and_joltages ->
    let buttons, _ = parse_buttons_and_joltages buttons_and_joltages in
    parse_flip_target target, buttons
  | _ -> failwith (Printf.sprintf "Invalid line '%s'" line)
;;

let parse_two line : Button.t list * int iarray =
  match String.split_on_char ' ' line with
  | _ :: buttons_and_joltages ->
    let buttons, joltages = parse_buttons_and_joltages buttons_and_joltages in
    buttons, joltages
  | _ -> failwith (Printf.sprintf "Invalid line '%s'" line)
;;

let parse_equation line =
  let buttons, joltages = parse_two line in
  Solver_Two.of_buttons_and_joltages buttons joltages
;;

let solve_part_one lines =
  List.map parse_one lines
  |> List.map (fun (start, buttons) -> Solver_One.solve start buttons)
  |> Shared.Reduction.AddInt.reduce_list
;;

let solve_part_two lines =
  List.map parse_two lines
  |> List.map (fun (buttons, joltages) ->
    Solver_Two.MathProg.solve (Solver_Two.of_buttons_and_joltages buttons joltages))
  |> Shared.Reduction.AddInt.reduce_list
;;

let solve_part_two_simplex lines =
  List.map parse_two lines
  |> List.map (fun (buttons, joltages) ->
    Solver_Two.Simplex.minimize (Solver_Two.of_buttons_and_joltages buttons joltages))
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
  [%expect {| 17424 |}]
;;

let%expect_test "Example Part Two" =
  let solution = solve_part_two example_input in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 33 |}]
;;

let%expect_test "Example Part Two" =
  let solution =
    solve_part_two_simplex
      [ "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
      ; "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
        (* ; "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}" *)
      ]
  in
  print_endline @@ Printf.sprintf "%d" solution;
  [%expect {| 22 |}]
;;

let%expect_test "Equation" =
  let eqs = List.map parse_equation example_input in
  List.iter (Fun.compose print_endline Solver_Two.MathProg.program) eqs;
  [%expect
    {|
    # start of program
    # minimizes the value: sum
    var a integer;
    var b integer;
    var c integer;
    var d integer;
    var e integer;
    var f integer;
    minimize sum: a+b+c+d+e+f;
    s.t. eq_0: e + f == 3;
    s.t. eq_1: b + f == 5;
    s.t. eq_2: c + d + e == 4;
    s.t. eq_3: a + b + d == 7;
    s.t. a_positive: a >= 0;
    s.t. b_positive: b >= 0;
    s.t. c_positive: c >= 0;
    s.t. d_positive: d >= 0;
    s.t. e_positive: e >= 0;
    s.t. f_positive: f >= 0;
    solve;
    printf "%d\n",sum;
    # end of program

    # start of program
    # minimizes the value: sum
    var a integer;
    var b integer;
    var c integer;
    var d integer;
    var e integer;
    minimize sum: a+b+c+d+e;
    s.t. eq_0: a + c + d == 7;
    s.t. eq_1: d + e == 5;
    s.t. eq_2: a + b + d + e == 12;
    s.t. eq_3: a + b + e == 7;
    s.t. eq_4: a + c + e == 2;
    s.t. a_positive: a >= 0;
    s.t. b_positive: b >= 0;
    s.t. c_positive: c >= 0;
    s.t. d_positive: d >= 0;
    s.t. e_positive: e >= 0;
    solve;
    printf "%d\n",sum;
    # end of program

    # start of program
    # minimizes the value: sum
    var a integer;
    var b integer;
    var c integer;
    var d integer;
    minimize sum: a+b+c+d;
    s.t. eq_0: a + b + c == 10;
    s.t. eq_1: a + c + d == 11;
    s.t. eq_2: a + c + d == 11;
    s.t. eq_3: a + b == 5;
    s.t. eq_4: a + b + c == 10;
    s.t. eq_5: c == 5;
    s.t. a_positive: a >= 0;
    s.t. b_positive: b >= 0;
    s.t. c_positive: c >= 0;
    s.t. d_positive: d >= 0;
    solve;
    printf "%d\n",sum;
    # end of program
    |}]
;;

let%expect_test "Simplex" =
  let module Simplex = Solver_Two.Simplex in
  let problem = parse_equation "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}" in
  let simplex = Simplex.init problem Maximize in
  Simplex.print simplex;
  [%expect
    {|
     x1| x2| x3| x4| x5| x6| a1| a2| a3| a4|    z|  b
    --------------------------------------------------
      0|  0|  0|  0|  1|  1|  1|  0|  0|  0|    0|  3
    --------------------------------------------------
      0|  1|  0|  0|  0|  1|  0|  1|  0|  0|    0|  5
    --------------------------------------------------
      0|  0|  1|  1|  1|  0|  0|  0|  1|  0|    0|  4
    --------------------------------------------------
      1|  1|  0|  1|  0|  0|  0|  0|  0|  1|    0|  7
    --------------------------------------------------
     -1| -1| -1| -1| -1| -1|  0|  0|  0|  0|    1|  0
    --------------------------------------------------
    |}];
  let i, j = Simplex.find_pivot simplex in
  print_endline (Printf.sprintf "Next pivot: %d, %d" i j);
  [%expect {| Next pivot: 3, 0 |}];
  Simplex.step simplex;
  Simplex.print simplex;
  [%expect
    {|
     x1| x2| x3| x4| x5| x6| a1| a2| a3| a4|    z|  b
    --------------------------------------------------
      0|  0|  0|  0|  1|  1|  1|  0|  0|  0|    0|  3
    --------------------------------------------------
      0|  1|  0|  0|  0|  1|  0|  1|  0|  0|    0|  5
    --------------------------------------------------
      0|  0|  1|  1|  1|  0|  0|  0|  1|  0|    0|  4
    --------------------------------------------------
      1|  1|  0|  1|  0|  0|  0|  0|  0|  1|    0|  7
    --------------------------------------------------
      0|  0| -1|  0| -1| -1|  0|  0|  0|  1|    1|  7
    --------------------------------------------------
    |}];
  Simplex.step simplex;
  Simplex.print simplex;
  [%expect
    {|
     x1| x2| x3| x4| x5| x6| a1| a2| a3| a4|    z|  b
    --------------------------------------------------
      0|  0|  0|  0|  1|  1|  1|  0|  0|  0|    0|  3
    --------------------------------------------------
      0|  1|  0|  0|  0|  1|  0|  1|  0|  0|    0|  5
    --------------------------------------------------
      0|  0|  1|  1|  1|  0|  0|  0|  1|  0|    0|  4
    --------------------------------------------------
      1|  1|  0|  1|  0|  0|  0|  0|  0|  1|    0|  7
    --------------------------------------------------
      0|  0|  0|  1|  0| -1|  0|  0|  1|  1|    1| 11
    --------------------------------------------------
    |}];
  Simplex.step simplex;
  Simplex.print simplex;
  [%expect
    {|
     x1| x2| x3| x4| x5| x6| a1| a2| a3| a4|    z|  b
    --------------------------------------------------
      0|  0|  0|  0|  1|  1|  1|  0|  0|  0|    0|  3
    --------------------------------------------------
      0|  1|  0|  0| -1|  0| -1|  1|  0|  0|    0|  2
    --------------------------------------------------
      0|  0|  1|  1|  1|  0|  0|  0|  1|  0|    0|  4
    --------------------------------------------------
      1|  1|  0|  1|  0|  0|  0|  0|  0|  1|    0|  7
    --------------------------------------------------
      0|  0|  0|  1|  1|  0|  1|  0|  1|  1|    1| 14
    --------------------------------------------------
    |}];
  Simplex.step simplex;
  Simplex.print simplex;
  [%expect
    {|
     x1| x2| x3| x4| x5| x6| a1| a2| a3| a4|    z|  b
    --------------------------------------------------
      0|  0|  0|  0|  1|  1|  1|  0|  0|  0|    0|  3
    --------------------------------------------------
      0|  1|  0|  0| -1|  0| -1|  1|  0|  0|    0|  2
    --------------------------------------------------
      0|  0|  1|  1|  1|  0|  0|  0|  1|  0|    0|  4
    --------------------------------------------------
      1|  0|  0|  1|  1|  0|  1| -1|  0|  1|    0|  5
    --------------------------------------------------
      0|  0|  0|  1|  1|  0|  1|  0|  1|  1|    1| 14
    --------------------------------------------------
    |}];
  print_endline (Printf.sprintf "%b" (Simplex.is_solved simplex));
  [%expect {| true |}];
  print_endline (Printf.sprintf "max = %d" (Simplex.maximize problem));
  [%expect {| max = 14 |}];
  let dual = Simplex.dual problem in
  Simplex.print (Simplex.init problem Maximize);
  print_endline ">>>";
  Simplex.print dual;
  [%expect
    {|
     x1| x2| x3| x4| x5| x6| a1| a2| a3| a4|    z|  b
    --------------------------------------------------
      0|  0|  0|  0|  1|  1|  1|  0|  0|  0|    0|  3
    --------------------------------------------------
      0|  1|  0|  0|  0|  1|  0|  1|  0|  0|    0|  5
    --------------------------------------------------
      0|  0|  1|  1|  1|  0|  0|  0|  1|  0|    0|  4
    --------------------------------------------------
      1|  1|  0|  1|  0|  0|  0|  0|  0|  1|    0|  7
    --------------------------------------------------
     -1| -1| -1| -1| -1| -1|  0|  0|  0|  0|    1|  0
    --------------------------------------------------
    >>>
     x1| x2| x3| x4| a1| a2| a3| a4| a5| a6|    z|  b
    --------------------------------------------------
      0|  0|  0|  1|  1|  0|  0|  0|  0|  0|    0|  1
    --------------------------------------------------
      0|  1|  0|  1|  0|  1|  0|  0|  0|  0|    0|  1
    --------------------------------------------------
      0|  0|  1|  0|  0|  0|  1|  0|  0|  0|    0|  1
    --------------------------------------------------
      0|  0|  1|  1|  0|  0|  0|  1|  0|  0|    0|  1
    --------------------------------------------------
      1|  0|  1|  0|  0|  0|  0|  0|  1|  0|    0|  1
    --------------------------------------------------
      1|  1|  0|  0|  0|  0|  0|  0|  0|  1|    0|  1
    --------------------------------------------------
     -3| -5| -4| -7|  0|  0|  0|  0|  0|  0|    1|  0
    --------------------------------------------------
    |}];
  Simplex.step dual;
  Simplex.print dual;
  [%expect
    {|
     x1| x2| x3| x4| a1| a2| a3| a4| a5| a6|    z|  b
    --------------------------------------------------
      0|  0|  0|  1|  1|  0|  0|  0|  0|  0|    0|  1
    --------------------------------------------------
      0|  1|  0|  0| -1|  1|  0|  0|  0|  0|    0|  0
    --------------------------------------------------
      0|  0|  1|  0|  0|  0|  1|  0|  0|  0|    0|  1
    --------------------------------------------------
      0|  0|  1|  0| -1|  0|  0|  1|  0|  0|    0|  0
    --------------------------------------------------
      1|  0|  1|  0|  0|  0|  0|  0|  1|  0|    0|  1
    --------------------------------------------------
      1|  1|  0|  0|  0|  0|  0|  0|  0|  1|    0|  1
    --------------------------------------------------
     -3| -5| -4|  0|  7|  0|  0|  0|  0|  0|    1|  7
    --------------------------------------------------
    |}];
  Simplex.step dual;
  Simplex.print dual;
  [%expect
    {|
     x1| x2| x3| x4| a1| a2| a3| a4| a5| a6|    z|  b
    --------------------------------------------------
      0|  0|  0|  1|  1|  0|  0|  0|  0|  0|    0|  1
    --------------------------------------------------
      0|  1|  0|  0| -1|  1|  0|  0|  0|  0|    0|  0
    --------------------------------------------------
      0|  0|  1|  0|  0|  0|  1|  0|  0|  0|    0|  1
    --------------------------------------------------
      0|  0|  1|  0| -1|  0|  0|  1|  0|  0|    0|  0
    --------------------------------------------------
      1|  0|  1|  0|  0|  0|  0|  0|  1|  0|    0|  1
    --------------------------------------------------
      1|  0|  0|  0|  1| -1|  0|  0|  0|  1|    0|  1
    --------------------------------------------------
     -3|  0| -4|  0|  2|  5|  0|  0|  0|  0|    1|  7
    --------------------------------------------------
    |}];
  Simplex.step dual;
  Simplex.print dual;
  [%expect
    {|
     x1| x2| x3| x4| a1| a2| a3| a4| a5| a6|    z|  b
    --------------------------------------------------
      0|  0|  0|  1|  1|  0|  0|  0|  0|  0|    0|  1
    --------------------------------------------------
      0|  1|  0|  0| -1|  1|  0|  0|  0|  0|    0|  0
    --------------------------------------------------
      0|  0|  0|  0|  1|  0|  1| -1|  0|  0|    0|  1
    --------------------------------------------------
      0|  0|  1|  0| -1|  0|  0|  1|  0|  0|    0|  0
    --------------------------------------------------
      1|  0|  0|  0|  1|  0|  0| -1|  1|  0|    0|  1
    --------------------------------------------------
      1|  0|  0|  0|  1| -1|  0|  0|  0|  1|    0|  1
    --------------------------------------------------
     -3|  0|  0|  0| -2|  5|  0|  4|  0|  0|    1|  7
    --------------------------------------------------
    |}];
  Simplex.step dual;
  Simplex.print dual;
  [%expect
    {|
     x1| x2| x3| x4| a1| a2| a3| a4| a5| a6|    z|  b
    --------------------------------------------------
      0|  0|  0|  1|  1|  0|  0|  0|  0|  0|    0|  1
    --------------------------------------------------
      0|  1|  0|  0| -1|  1|  0|  0|  0|  0|    0|  0
    --------------------------------------------------
      0|  0|  0|  0|  1|  0|  1| -1|  0|  0|    0|  1
    --------------------------------------------------
      0|  0|  1|  0| -1|  0|  0|  1|  0|  0|    0|  0
    --------------------------------------------------
      1|  0|  0|  0|  1|  0|  0| -1|  1|  0|    0|  1
    --------------------------------------------------
      0|  0|  0|  0|  0| -1|  0|  1| -1|  1|    0|  0
    --------------------------------------------------
      0|  0|  0|  0|  1|  5|  0|  1|  3|  0|    1| 10
    --------------------------------------------------
    |}];
  print_endline (Printf.sprintf "%b" (Simplex.is_solved dual));
  [%expect {| true |}];
  print_endline (Printf.sprintf "min = %d" (Simplex.minimize problem));
  [%expect {| min = 10 |}]
;;

let%expect_test "Complex Simplex" =
  let module Simplex = Solver_Two.Simplex in
  let problem =
    parse_equation "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
  in
  let simplex = Simplex.init problem Minimize in
  Simplex.print simplex;
  [%expect
    {|
     x1| x2| x3| x4| a1| a2| a3| a4| a5| a6|    z|  b
    --------------------------------------------------
      1|  1|  1|  0|  1|  0|  0|  0|  0|  0|    0| 10
    --------------------------------------------------
      1|  0|  1|  1|  0|  1|  0|  0|  0|  0|    0| 11
    --------------------------------------------------
      1|  0|  1|  1|  0|  0|  1|  0|  0|  0|    0| 11
    --------------------------------------------------
      1|  1|  0|  0|  0|  0|  0|  1|  0|  0|    0|  5
    --------------------------------------------------
      1|  1|  1|  0|  0|  0|  0|  0|  1|  0|    0| 10
    --------------------------------------------------
      0|  0|  1|  0|  0|  0|  0|  0|  0|  1|    0|  5
    --------------------------------------------------
      1|  1|  1|  1|  0|  0|  0|  0|  0|  0|    1|  0
    --------------------------------------------------
    |}];
  Simplex.step simplex;
  Simplex.print simplex;
  [%expect
    {|
     x1| x2| x3| x4| a1| a2| a3| a4| a5| a6|    z|  b
    --------------------------------------------------
      1|  1|  1|  0|  1|  0|  0|  0|  0|  0|    0| 10
    --------------------------------------------------
      0| -1|  0|  1| -1|  1|  0|  0|  0|  0|    0|  1
    --------------------------------------------------
      0| -1|  0|  1| -1|  0|  1|  0|  0|  0|    0|  1
    --------------------------------------------------
      0|  0| -1|  0| -1|  0|  0|  1|  0|  0|    0| -5
    --------------------------------------------------
      0|  0|  0|  0| -1|  0|  0|  0|  1|  0|    0|  0
    --------------------------------------------------
      0|  0|  1|  0|  0|  0|  0|  0|  0|  1|    0|  5
    --------------------------------------------------
      0|  0|  0|  1| -1|  0|  0|  0|  0|  0|    1|-10
    --------------------------------------------------
    |}];
  Simplex.step simplex;
  Simplex.print simplex;
  [%expect
    {|
     x1| x2| x3| x4| a1| a2| a3| a4| a5| a6|    z|  b
    --------------------------------------------------
      0|  0|  0|  0|  0|  0|  0|  0|  0|  0|    0|  1
    --------------------------------------------------
      0| -1|  0|  1| -1|  1|  0|  0|  0|  0|    0|  0
    --------------------------------------------------
      0| -1|  0|  1| -1|  0|  1|  0|  0|  0|    0|  0
    --------------------------------------------------
      0|  0| -1|  0| -1|  0|  0|  1|  0|  0|    0|  0
    --------------------------------------------------
      0|  0|  0|  0| -1|  0|  0|  0|  1|  0|    0|  0
    --------------------------------------------------
      0|  0|  1|  0|  0|  0|  0|  0|  0|  1|    0|  0
    --------------------------------------------------
      0|  0|  0|  1| -1|  0|  0|  0|  0|  0|    1|  0
    --------------------------------------------------
    |}]
;;
