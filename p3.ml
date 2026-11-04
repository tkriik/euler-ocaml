let int_sqrt number = int_of_float (sqrt (float number))

let is_prime number =
  let result = ref (number >= 2) in
  let factor = ref 2 in
  while !result && !factor <= (int_sqrt number) do
    if number mod !factor = 0 then result := false;
    incr factor
  done;
  !result

let max_int_in list = List.fold_left max 0 list

let input = 600851475143
let ans =
  List.init (int_sqrt input) succ |>
  List.filter is_prime |>
  List.filter (fun n -> input mod n = 0) |>
  max_int_in
