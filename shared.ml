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

module Reduction = struct
  module type S = sig
    type t

    val neutral : t
    val combine : t -> t -> t
  end

  module type A = sig
    type t

    val zero : t
    val add : t -> t -> t
  end

  module type P = sig
    type t

    val one : t
    val mul : t -> t -> t
  end

  module Make (S : S) = struct
    let reduce_list l = List.fold_left S.combine S.neutral l
    let reduce_array l = Array.fold_left S.combine S.neutral l
    let reduce_seq l = Seq.fold_left S.combine S.neutral l
  end

  module Addition (A : A) : S with type t = A.t = struct
    type t = A.t

    let neutral = A.zero
    let combine = A.add
  end

  module Product (A : P) : S with type t = A.t = struct
    type t = A.t

    let neutral = A.one
    let combine = A.mul
  end

  module AddInt64 = Make (Addition (Int64))
  module AddInt = Make (Addition (Int))
  module ProdInt64 = Make (Product (Int64))
  module ProdInt = Make (Product (Int))

  module MaxInt = Make (struct
      type t = Int.t

      let combine a b = Int.max a b
      let neutral = Int.min_int
    end)

  module MaxInt64 = Make (struct
      type t = Int64.t

      let combine a b = Int64.max a b
      let neutral = Int64.min_int
    end)
end
