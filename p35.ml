let rotations n =
  let ds = string_of_int n in
  let len = String.length ds in
  let cycle = Seq.cycle (String.to_seq ds) in
  List.init len (fun n -> n)
  |> List.map (fun offset ->
    Seq.drop offset cycle |> Seq.take len |> String.of_seq |> int_of_string)
;;

let is_circular_prime n = List.for_all Util.is_prime (rotations n)

let _ =
  Util.gen_primes 1_000_000
  |> List.filter is_circular_prime
  |> List.length
  |> Printf.printf "%d\n"
;;
