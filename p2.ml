let lim = 4_000_000
let cur = ref 1
let nxt = ref 2
let sum = ref 0

let () = while !cur <= lim do
  if !cur mod 2 = 0 then sum := !sum + !cur;
  let tmp = !nxt in
  nxt := !cur + !nxt;
  cur := tmp
done

let ans = !sum
