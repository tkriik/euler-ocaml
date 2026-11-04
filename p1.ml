let lim = 1000
let sum = ref 0
let () = for i = 3 to lim - 1 do
  if i mod 3 = 0 || i mod 5 = 0 then sum := !sum + i
done

let ans = !sum
