let is_divisible_up_to lim n =
  let res = ref true in
  let i = ref 1 in
  while !res && !i <= lim do
    res := (n mod !i) = 0;
    incr i
  done;
  !res

let lim = 20
let num = ref (lim * lim)
let ans =
  while not (is_divisible_up_to lim !num) do
    incr num
  done;
  !num
