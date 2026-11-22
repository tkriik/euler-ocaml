let concat_prod lim n =
  List.init lim succ |> List.map (fun i -> string_of_int (n * i)) |> String.concat ""
;;

let unord_hash s = String.to_seq s |> Seq.fold_left (fun hash c -> hash * int_of_char c) 1
let pan9_hash = unord_hash "123456789"
let is_pan9 s = String.length s = 9 && unord_hash s = pan9_hash

let _ =
  let max_prod = ref 0 in
  for lim = 9 downto 2 do
    let prod =
      Seq.ints 1
      |> Seq.map (fun n -> concat_prod lim n)
      |> Seq.take_while (fun s -> String.length s <= 9)
      |> Seq.filter is_pan9
      |> Seq.map int_of_string
      |> Seq.fold_left (fun max_p p -> max max_p p) 1
    in
    max_prod := max !max_prod prod
  done;
  Printf.printf "%d\n" !max_prod
;;
