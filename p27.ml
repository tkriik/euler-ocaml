let f n a b = (n * n) + (a * n) + b

let count_primes a b =
  Seq.iterate succ 0
  |> Seq.map (fun n -> f n a b)
  |> Seq.take_while Util.is_prime
  |> Seq.length
;;

let _ =
  let max_cnt = ref 0 in
  let product = ref 0 in
  for a = -999 to 999 do
    for b = -1000 to 1000 do
      let cnt = count_primes a b in
      if !max_cnt < cnt
      then (
        max_cnt := cnt;
        product := a * b)
    done
  done;
  Printf.printf "%d\n" !product
;;
