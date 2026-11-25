let rec fact = function
  | 0 -> Z.one
  | n when 0 < n -> Z.mul (Z.of_int n) (fact (n - 1))
  | _ -> invalid_arg "n must be positive"
;;

let binom n r =
  let nf = fact n in
  let rf = fact r in
  let n_sub_r = fact (n - r) in
  Z.div nf (Z.mul rf n_sub_r)
;;

let _ =
  let minimum = Z.of_int 1_000_000 in
  let cnt = ref 0 in
  for n = 1 to 100 do
    for r = 1 to n do
      let ways = binom n r in
      if Z.gt ways minimum then incr cnt
    done
  done;
  Printf.printf "%d\n" !cnt
;;
