let d n =
  List.init (n / 2) succ
  |> List.fold_left (fun acc m -> if n mod m = 0 then acc + m else acc) 0

let limit = 10_000
let cache = Array.make (4 * limit) None

let is_amicable n =
  match cache.(n) with
  | None ->
      let dn = d n in
      let ddn = d dn in
      let b = n <> dn && n = ddn in
      cache.(n) <- Some b;
      cache.(dn) <- Some b;
      b
  | Some b -> b

let ans =
  List.init 10_000 succ
  |> List.fold_left (fun acc n -> if is_amicable n then acc + n else acc) 0

let () = Printf.printf "%d\n" ans
