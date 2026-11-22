let digit_to_int c = Char.code c - Char.code '0'

let is_pandigital x y =
  let actual = ref 0x000 in
  (* 00 0000 0000 *)
  let expected = 0x3FE in
  (* 11 1111 1110; 1 hit for 1-9, no hits for 0 *)
  let digits = string_of_int x ^ string_of_int y ^ string_of_int (x * y) in
  let exception NonPandigital in
  try
    String.iter
      (fun c ->
         let d = digit_to_int c in
         if (!actual lsr d) land 0x1 = 1
         then raise NonPandigital
         else actual := !actual lor (1 lsl d))
      digits;
    !actual = expected
  with
  | NonPandigital -> false
;;

let _ =
  let products = Hashtbl.create 32 in
  for x = 1 to 999 do
    for y = x + 1 to 9999 do
      if is_pandigital x y then Hashtbl.replace products (x * y) true
    done
  done;
  let sum = Hashtbl.fold (fun product _ acc -> acc + product) products 0 in
  Printf.printf "%d\n" sum
;;
