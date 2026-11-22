let rec dhash n = if n = 0 then 1 else Int.hash (n mod 10) * dhash (n / 10)
let dh2 = dhash 12
let dh3 = dhash 123
let dh4 = dhash 1234
let dh5 = dhash 12345
let dh6 = dhash 123456
let dh7 = dhash 1234567

let is_panint lim n =
  let dhn = dhash n in
  match lim with
  | 2 -> dhn = dh2
  | 3 -> dhn = dh3
  | 4 -> dhn = dh4
  | 5 -> dhn = dh5
  | 6 -> dhn = dh6
  | 7 -> dhn = dh7
  | _ -> invalid_arg "lim < 2 || 7 < lim"
;;

let _ =
  let exception Found of int in
  (try
     for lim = 7 downto 2 do
       for n = Util.int_pow 10 lim - 1 downto Util.int_pow 10 (lim - 1) + 1 do
         if is_panint lim n && Util.is_prime n then raise (Found n)
       done
     done
   with
   | Found n ->
     Printf.printf "%d\n" n;
     exit 0);
  print_endline "none found";
  exit 1
;;
