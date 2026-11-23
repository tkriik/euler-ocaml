let is_prime = Util.is_prime

let odd_comps =
  Seq.unfold (fun n -> Some (n, n + 2)) 3 |> Seq.filter (fun n -> not (is_prime n))
;;

let x2sq n = 2 * (n * n)

let has_prop n =
  Seq.iterate succ 1
  |> Seq.map x2sq
  |> Seq.take_while (fun x2 -> 1 < n - x2)
  |> Seq.filter (fun x2 -> is_prime (n - x2))
  |> List.of_seq
  |> fun l -> not (List.is_empty l)
;;

let _ =
  odd_comps
  |> Seq.drop_while has_prop
  |> Seq.take 1
  |> List.of_seq
  |> List.hd
  |> Printf.printf "%d\n"
;;
