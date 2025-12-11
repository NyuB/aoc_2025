module StringMap = Map.Make (String)

type leds = string

module Button = struct
  type t = int array

  let sort_descending_length ts =
    List.sort (fun a b -> Int.neg @@ Int.compare (Array.length a) (Array.length b)) ts
  ;;

  let flip = function
    | '#' -> '.'
    | '.' -> '#'
    | c -> failwith (Printf.sprintf "Invalid state character %c" c)
  ;;

  let apply_leds button leds =
    let res = Bytes.init (String.length leds) (String.get leds) in
    Array.iter (fun i -> Bytes.set res i (flip (Bytes.get res i))) button;
    Bytes.to_string res
  ;;
end

(** Solves part one exploring all states with a dijktsra-like algorithm *)
module Solver_One = struct
  type presses = int StringMap.t

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

  let neighbours (buttons : Button.t list) state =
    List.map (fun b -> Button.apply_leds b state) buttons
  ;;

  let solve target buttons =
    let start = String.init (String.length target) (Fun.const '.') in
    let q = Q.of_list [ Item.{ leds = start; presses = 0 } ] in
    let presses = StringMap.empty in
    shortest_flip_sequence (neighbours buttons) target q presses
  ;;
end

module Solver_Two = struct
  (** variables' coefficients and sum *)
  type equation = int array * int

  type t =
    { nb_variables : int
    ; equations : equation array
    }

  let of_buttons_and_joltages (buttons : Button.t list) joltages =
    let nb_variables = List.length buttons in
    let nb_equations = Array.length joltages in
    let equations = Array.init nb_equations (fun _ -> Array.make nb_variables 0) in
    List.iteri (fun bi b -> Array.iter (fun c -> equations.(c).(bi) <- 1) b) buttons;
    { nb_variables; equations = Array.combine equations joltages }
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
        Array.mapi (fun i coeff -> coeff, var_coeff_in_sum i coeff) vars
        |> Array.to_list
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
      @ (Array.mapi equation_constraint t.equations |> Array.to_list)
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
end

let without_bracket s = String.sub s 1 (String.length s - 2)
let parse_flip_target target = without_bracket target

let parse_button button : Button.t =
  String.split_on_char ',' (without_bracket button)
  |> List.map int_of_string
  |> Array.of_list
;;

let parse_joltages joltages =
  String.split_on_char ',' (without_bracket joltages)
  |> List.map int_of_string
  |> Array.of_list
;;

let parse_buttons_and_joltages buttons_and_joltages =
  match List.rev buttons_and_joltages with
  | [] -> failwith "Needs at least joltages"
  | joltages :: buttons ->
    List.map parse_button buttons |> List.rev, parse_joltages joltages
;;

let parse_one line =
  match String.split_on_char ' ' line with
  | target :: buttons_and_joltages ->
    let buttons, _ = parse_buttons_and_joltages buttons_and_joltages in
    parse_flip_target target, buttons
  | _ -> failwith (Printf.sprintf "Invalid line '%s'" line)
;;

let parse_two line : Button.t list * int array =
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
