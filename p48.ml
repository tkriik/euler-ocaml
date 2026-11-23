let sum_self_powers up_to =
  List.init up_to succ
  |> List.map (fun n -> Z.pow (Z.of_int n) n)
  |> List.fold_left Z.add Z.zero
;;

let _ =
  let s = Z.to_string (sum_self_powers 1_000) in
  let len = String.length s in
  let ds = String.sub s (len - 10) 10 in
  Printf.printf "%s\n" ds
;;
