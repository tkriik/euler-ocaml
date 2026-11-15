let d n =
  List.init (n / 2) succ
  |> List.fold_left (fun acc m -> if n mod m = 0 then acc + m else acc) 0

let is_abundant n = n < d n
let limit = 28123
let sieve = Array.make limit false
let abundants = List.init limit succ |> List.filter is_abundant

let rec run_sieve = function
  | [] -> ()
  | hd :: tl as lst ->
      List.iteri
        (fun i n ->
          let j = hd + n - 1 in
          if j < limit then sieve.(j) <- true)
        lst;
      run_sieve tl

let ans =
  run_sieve abundants;
  let sum = ref 0 in
  Array.iteri (fun i b -> if not b then sum := !sum + (i + 1)) sieve;
  Printf.printf "%d\n" !sum;
  !sum
