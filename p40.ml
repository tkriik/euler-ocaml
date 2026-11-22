let _ =
  let lim = 1_000_000 in
  let buf = Buffer.create lim in
  for i = 1 to lim do
    Buffer.add_string buf (string_of_int i)
  done;
  let d n = Char.Ascii.digit_to_int (Buffer.nth buf (n - 1)) in
  let res = d 1 * d 10 * d 100 * d 1000 * d 10_000 * d 100_000 * d 1_000_000 in
  Printf.printf "%d\n" res
;;
