module IntSet = Set.Make (Int)

let has_4 n =
  let set = IntSet.of_list (Util.prime_factors n) in
  IntSet.cardinal set = 4
;;

let has_4_4s n = List.for_all has_4 [ n; n + 1; n + 2; n + 3 ]

let _ =
  Seq.iterate succ 1
  |> Seq.filter has_4_4s
  |> Seq.take 1
  |> List.of_seq
  |> List.hd
  |> Printf.printf "%d\n"
;;
