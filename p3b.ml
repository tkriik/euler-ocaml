let num = ref 600851475143
let res = ref !num
let () =
  for i = 2 to int_of_float (sqrt (float !num)) do
    while !num > 1 && !num mod i = 0 do
      num := !num / i;
      res := i
    done
  done

let ans = !res
