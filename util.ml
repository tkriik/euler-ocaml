let int_pow n exp = int_of_float (float_of_int n ** float_of_int exp)
let rec gcd n d = if d = 0 then n else gcd d (n mod d)

let gen_primes count =
  if count < 2
  then []
  else (
    let composite = Array.make (count + 1) false in
    let p = ref 2 in
    while !p <= count / !p do
      if not composite.(!p)
      then (
        let multiple = ref (!p * !p) in
        while !multiple <= count do
          composite.(!multiple) <- true;
          multiple := !multiple + !p
        done);
      incr p
    done;
    let result = ref [] in
    for n = count downto 2 do
      if not composite.(n) then result := n :: !result
    done;
    !result)
;;

let is_prime n =
  if n = 2 || n = 3
  then true
  else if n < 2 || n mod 2 = 0 || n mod 3 = 0
  then false
  else (
    let rec test d =
      if d > n / d
      then true
      else if n mod d = 0 || n mod (d + 2) = 0
      then false
      else test (d + 6)
    in
    test 5)
;;
