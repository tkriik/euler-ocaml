let permute arr =
  let pivot = ref (-1) in
  let i = ref (Array.length arr - 2) in
  while !pivot = -1 && 0 <= !i do
    if arr.(!i) < arr.(!i + 1) then pivot := !i;
    decr i
  done;
  let next = ref (-1) in
  for i = Array.length arr - 1 downto !pivot + 1 do
    if arr.(!pivot) < arr.(i) then
      if !next = -1 then next := i else if arr.(i) < arr.(!next) then next := i
  done;
  let tmp = arr.(!pivot) in
  arr.(!pivot) <- arr.(!next);
  arr.(!next) <- tmp;
  let i = ref (!pivot + 1) in
  let j = ref (Array.length arr - 1) in
  while !i < !j do
    let tmp = arr.(!i) in
    arr.(!i) <- arr.(!j);
    arr.(!j) <- tmp;
    incr i;
    decr j
  done;
  ()

let start = Array.of_list [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
let nth = 1_000_000

let ans =
  for i = 1 to nth - 1 do
    permute start
  done;
  for i = 0 to Array.length start - 1 do
    print_int start.(i)
  done;
  print_endline ""
