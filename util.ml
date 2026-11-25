let int_sqrt n = float_of_int n |> sqrt |> int_of_float
let int_pow n exp = int_of_float (float_of_int n ** float_of_int exp)
let int_log10 n = n |> float_of_int |> log10 |> int_of_float
let rec gcd n d = if d = 0 then n else gcd d (n mod d)

let digits = function
  | n when abs n < 10 -> [ n ]
  | n ->
    let rec aux n' = if n' = 0 then [] else (n' mod 10) :: aux (n' / 10) in
    List.rev (aux n)
;;

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

let next_prime n =
  Seq.iterate (fun n' -> n' + 1) (n + 1)
  |> Seq.filter is_prime
  |> Seq.take 1
  |> List.of_seq
  |> List.hd
;;

let prime_factors n =
  let fs = ref [] in
  let n' = ref n in
  while !n' mod 2 = 0 do
    fs := 2 :: !fs;
    n' := !n' / 2
  done;
  let i = ref 3 in
  while !i * !i <= !n' do
    while !n' mod !i = 0 do
      fs := !i :: !fs;
      n' := !n' / !i
    done;
    i := !i + 2
  done;
  if !n' > 1 then fs := !n' :: !fs;
  !fs
;;
