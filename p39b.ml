let square n = n * n

let gen_triple m n =
  match m, n with
  | m, n when m > n -> square m - square n, 2 * m * n, square m + square n
  | _ -> invalid_arg "m <= n"
;;

let _ =
  let max_p = ref (-1) in
  let max_sols = ref 0 in
  for p = 1000 downto 12 do
    let sols = ref 0 in
    for n = 1 to Util.int_sqrt p do
      for m = n + 1 to Util.int_sqrt p do
        let a, b, c = gen_triple m n in
        for k = 1 to Util.int_sqrt p do
          let ka, kb, kc = k * a, k * b, k * c in
          if ka + kb + kc = p && Util.gcd m n = 1 && m mod 2 <> n mod 2 then incr sols
        done
      done
    done;
    if !max_sols < !sols
    then (
      max_sols := !sols;
      max_p := p)
  done;
  Printf.printf "%d -> %d solutions\n" !max_p !max_sols
;;
