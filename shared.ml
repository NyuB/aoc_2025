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

  module AddBigint = Make (struct
      type t = Bigint.t

      let combine a b = Bigint.(a + b)
      let neutral = Bigint.zero
    end)

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

module Point = struct
  type t =
    { i : int
    ; j : int
    }

  let compare { i = ia; j = ja } { i = ib; j = jb } =
    let compare_i = Int.compare ia ib in
    if compare_i != 0 then compare_i else Int.compare ja jb
  ;;

  let show { i; j } = Printf.sprintf "(%d, %d)" i j
  let left { i; j } = { i; j = j - 1 }
  let right { i; j } = { i; j = j + 1 }
  let equal a b = a.i = b.i && a.j = b.j
  let hash a = Int.hash (a.i * 13 * a.j * 17)

  let parse_xy s =
    match String.split_on_char ',' s with
    | [ x; y ] -> { i = int_of_string y; j = int_of_string x }
    | _ -> failwith (Printf.sprintf "Invalid point representation %s" s)
  ;;
end
