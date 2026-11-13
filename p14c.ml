let collatz n = if n mod 2 = 0 then n / 2 else 3 * n + 1

let collatz_len n =
  let n' = ref n in
  let len = ref 1 in
  while !n' > 1 do
    n' := collatz !n';
    incr len
  done;
  !len

let limit = 1_000_000
let ans =
  let pair_cmp (nmax, lenmax) (n, len) =
    if len < lenmax then (nmax, lenmax) else (n, len) in
  List.init limit succ
  |> List.map (fun n -> n, collatz_len n)
  |> List.fold_left pair_cmp (1, 1)
  |> fst

let () = Printf.printf "%d\n" ans
