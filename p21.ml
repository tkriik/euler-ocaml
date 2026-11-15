let d n =
  List.init (n / 2) succ
  |> List.fold_left (fun acc m -> if n mod m = 0 then acc + m else acc) 0

let is_amicable n =
  let dn = d n in
  let ddn = d dn in
  n <> dn && n = ddn

let ans =
  List.init 10_000 succ |> List.filter is_amicable |> List.fold_left ( + ) 0

let () = Printf.printf "%d\n" ans
