let w = 20 + 1 (* +1 for corners *)
let h = 20 + 1
let grid =
  List.init h succ
  |> List.map (fun y -> Array.make w (-1))
  |> Array.of_list

let () = grid.(0).(0) <- 1 (* seed *)

let get = function
  | (x, _) when x < 0 || w <= x -> 0
  | (_, y) when y < 0 || h <= y -> 0
  | (x, y) -> grid.(y).(x)

let rec compute_routes (x, y) = match get (x, y) with
  | -1 ->
      let up = get (x, y - 1) in
      let left = get (x - 1, y) in
      let n = up + left in
      grid.(y).(x) <- n
  | n -> ()

let () =
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      compute_routes (x, y);
    done
  done;
  Printf.printf "%d\n" ans
