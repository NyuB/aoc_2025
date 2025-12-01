let read_input_lines file =
  let ic = open_in file in
  let close () = close_in ic in
  Fun.protect ~finally:close
  @@ fun () ->
  let rec aux acc () =
    try aux (input_line ic :: acc) () with
    | End_of_file -> List.rev acc
  in
  aux [] ()
;;

let read_input_one_line file =
  match read_input_lines file with
  | [ line ] -> line
  | [] -> failwith (Printf.sprintf "Empty input file '%s'" file)
  | _ -> failwith (Printf.sprintf "More than one line in input file '%s'" file)
;;
