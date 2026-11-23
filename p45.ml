module IntSet = Set.Make (Int)

let tri n = n * (n + 1) / 2
let penta n = n * ((3 * n) - 1) / 2
let hexa n = n * ((2 * n) - 1)

let _ =
  let batch = ref 1024 in
  let exception Found of int in
  try
    while true do
      let ns = List.init !batch (fun n -> n + 2) in
      let tset = IntSet.of_list (List.map tri ns) in
      let pset = IntSet.of_list (List.map penta ns) in
      let hset = IntSet.of_list (List.map hexa ns) in
      let inter = IntSet.inter (IntSet.inter tset pset) hset in
      let rec find set =
        match IntSet.min_elt_opt set with
        | None -> batch := !batch * 2
        | Some n when n = 40755 -> find (IntSet.remove n set)
        | Some n -> raise (Found n)
      in
      find inter
    done
  with
  | Found n -> Printf.printf "batch=%d  %d\n" !batch n
;;
