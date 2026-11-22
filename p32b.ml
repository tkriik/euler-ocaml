let digits n =
  let rec aux n () =
    if n < 10 then Seq.Cons (n, Seq.empty) else Seq.Cons (n mod 10, aux (n / 10))
  in
  aux n |> List.of_seq
;;

let is_pandigital x y =
  let exception NonPandigital in
  try
    (* 11 1111 1110; 1 hit for 1-9, no hits for 0 *)
    let expected = 0x3FE in
    let xds, yds, pds = digits x, digits y, digits (x * y) in
    let digits = xds @ yds @ pds in
    if List.length digits <> 9 then raise NonPandigital;
    let actual = List.fold_left (fun acc d -> acc lor (1 lsl d)) 0x000 digits in
    actual = expected
  with
  | NonPandigital -> false
;;

let _ =
  let products = Hashtbl.create 32 in
  for x = 1 to 9 do
    for y = 1000 to 9999 do
      if is_pandigital x y then Hashtbl.replace products (x * y) true
    done
  done;
  for x = 10 to 99 do
    for y = 100 to 999 do
      if is_pandigital x y then Hashtbl.replace products (x * y) true
    done
  done;
  let sum = Hashtbl.fold (fun product _ acc -> acc + product) products 0 in
  Printf.printf "%d\n" sum
;;
