let int_sqrt number = int_of_float (sqrt (float number))

let is_prime number =
  let result = ref (number >= 2) in
  let factor = ref 2 in
  while !result && !factor <= (int_sqrt number) do
    if number mod !factor = 0 then result := false;
    incr factor
  done;
  !result

let nth_prime n =
  let idx = ref 0 in
  let p = ref 1 in
  while !idx < n do
    if is_prime !p then
      incr idx
    idx <- idx + if is_prime
