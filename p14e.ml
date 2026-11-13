open Printf

let limit = 1_000_000
let cache_size = limit * 2
let lengths = Array.make cache_size (-1)
let collatz n = if n mod 2 = 0 then n / 2 else 3 * n + 1
let rec collatz_len n =
  if n < cache_size && lengths.(n) <> -1 then
    lengths.(n)
  else
    let len =
      if n = 1 then 1
      else 1 + collatz_len (collatz n)
    in
    if n < cache_size then lengths.(n) <- len;
    len

let ans =
  let pair_cmp (nmax, lenmax) (n, len) =
    if len < lenmax then (nmax, lenmax) else (n, len) in
  List.init limit succ
  |> List.map (fun n -> n, collatz_len n)
  |> List.fold_left pair_cmp (1, 1)
  |> fst
  |> printf "%d\n"
