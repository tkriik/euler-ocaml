let prime_lim = 1_000_000
let primes = Array.of_list (Util.gen_primes prime_lim)
let prime_cnt = Array.length primes

let test_range pos len =
  let sum = ref 0 in
  let skip = ref false in
  let exception Overflow in
  (try
     for i = pos to pos + len - 1 do
       let sum' = !sum + primes.(i) in
       if prime_lim <= sum' then raise Overflow;
       sum := sum'
     done
   with
   | Overflow -> skip := true);
  if !skip then !sum, false else !sum, Util.is_prime !sum
;;

let _ =
  let max_pos, max_len, max_sum = ref 0, ref 0, ref 0 in
  for pos = 0 to prime_cnt - 2 do
    Printf.printf "pos=%d\n%!" pos;
    for len = 0 to prime_cnt - pos - 1 do
      let sum, is_prime = test_range pos len in
      if is_prime && !max_len < len
      then (
        max_sum := sum;
        max_pos := pos;
        max_len := len)
    done
  done;
  Printf.printf "max_pos=%d max_len=%d max_sum=%d\n" !max_pos !max_len !max_sum
;;
