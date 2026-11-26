let digit_sum a b =
  Z.pow (Z.of_int a) b
  |> Z.to_string
  |> String.to_seq
  |> Seq.map Char.Ascii.digit_to_int
  |> Seq.fold_left ( + ) 0
;;

let pairs amax bmax =
  List.of_seq (Seq.product (Seq.init amax Fun.id) (Seq.init bmax Fun.id))
;;

let _ =
  pairs 100 100
  |> List.map (fun (a, b) -> digit_sum a b)
  |> List.fold_left (fun acc n -> if acc < n then n else acc) 0
  |> Printf.printf "%d\n"
;;
