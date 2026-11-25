module IntSet = Set.Make (Int)

let digit_set n = IntSet.of_list (Util.digits n)
let have_same_digits x y = IntSet.equal (digit_set x) (digit_set y)
let product_chain len n = List.init len (fun i -> n * (i + 1))

let is_same_digit_chain = function
  | [] | [ _ ] -> false
  | x :: xs -> List.fold_left (fun acc y -> acc && have_same_digits x y) true xs
;;

let _ =
  Seq.iterate succ 100_000
  |> Seq.map (product_chain 6)
  |> Seq.filter is_same_digit_chain
  |> Seq.take 1
  |> List.of_seq
  |> List.hd
  |> List.hd
  |> Printf.printf "%d\n"
;;
