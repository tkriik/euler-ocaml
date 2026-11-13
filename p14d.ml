let collatz n = if n mod 2 = 0 then n / 2 else 3 * n + 1

let limit = 1_000_000
let lengths = Hashtbl.create limit
let rec collatz_len n =
  let n' = ref n in
  let len = ref 1 in
  while !n' > 1 do
    n' := collatz !n';
    len :=
      match Hashtbl.find_opt lengths !n' with
      | Some len' -> n' := 1; len'
      | None ->
          let len' = 1 + collatz_len !n' in
          Hashtbl.replace lengths !n' len';
          len'
  done;
  !len

let ans =
  let pair_cmp (nmax, lenmax) (n, len) =
    if len < lenmax then (nmax, lenmax) else (n, len) in
  List.init limit succ
  |> List.map (fun n -> n, collatz_len n)
  |> List.fold_left pair_cmp (1, 1)
  |> fst

let () = Printf.printf "%d\n" ans
