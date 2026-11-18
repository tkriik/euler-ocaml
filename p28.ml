let next (num, ring, step) =
  let inc =
    match ring with
    | 1 -> 2
    | _ -> ring * 2
  in
  let ring', step' = if step = 4 then ring + 1, 1 else ring, step + 1 in
  num + inc, ring', step'
;;

let spiral w h =
  Seq.iterate next (1, 1, 1) |> Seq.take_while (fun (num, _, _) -> num <= w * h)
;;

let answer = spiral 1001 1001 |> Seq.fold_left (fun acc (num, _, _) -> acc + num) 0
