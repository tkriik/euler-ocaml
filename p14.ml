let collatz n = if n mod 2 = 0 then n / 2 else 3 * n + 1
let collatz_seq n = Seq.iterate collatz n |> Seq.take_while (fun n -> n > 1)

let limit = 1_000_000
let lengths = Hashtbl.create limit
let rec collatz_len n = match Hashtbl.find_opt lengths n with
  | Some len -> len
  | None ->
      let len = 1 + Seq.length (collatz_seq n) in
      Hashtbl.replace lengths n len;
      len

let ans =
  let pair_cmp (nmax, lenmax) (n, len) =
    if len < lenmax then (nmax, lenmax) else (n, len) in
  List.init limit succ
  |> List.map (fun n -> n, collatz_len n)
  |> List.fold_left pair_cmp (1, 1)
  |> fst

let () = Printf.printf "%d\n" ans
