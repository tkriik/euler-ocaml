let frac_eq (n1, d1) (n2, d2) = n1 * d2 = n2 * d1
let frac_mul (n1, d1) (n2, d2) = n1 * n2, d1 * d2
let is_nonzero (n, d) = n mod 10 <> 0 && d mod 10 <> 0

let is_cancelling (n1, d1) (n2, d2) =
  n1 mod 10 = d1 / 10 && n1 / 10 = n2 && d1 mod 10 = d2
;;

let _ =
  let fracs = ref [] in
  for n1 = 10 to 99 do
    for d1 = 99 downto n1 + 1 do
      for n2 = 1 to 9 do
        for d2 = 10 downto 2 do
          let a, b = (n1, d1), (n2, d2) in
          if is_nonzero a && is_nonzero b && is_cancelling a b && frac_eq a b
          then fracs := a :: !fracs
        done
      done
    done
  done;
  let n, d = List.fold_left (fun acc fr -> frac_mul acc fr) (1, 1) !fracs in
  Printf.printf "%d\n" (d / Util.gcd n d)
;;
