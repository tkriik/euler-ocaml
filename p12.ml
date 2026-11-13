let int_sqrt n = float n |> sqrt |> int_of_float
let divisor_count n =
  List.init (int_sqrt (n + 1)) succ
  |> List.map (fun i -> if n mod i = 0 then (if n / i = i then 1 else 2) else 0)
  |> List.fold_left (( + )) 0

let triangle n = n * (n + 1) / 2
let triangles = Seq.map triangle (Seq.ints 1)

let ans =
  match triangles |> Seq.filter (fun n -> divisor_count n > 500) |> Seq.uncons with
  | Some (n, _) -> n
  | None -> raise (Invalid_argument "fail")

let () = Printf.printf "%d\n" ans
