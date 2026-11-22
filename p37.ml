let rights n =
  let lst =
    Seq.unfold
      (function
        | None -> None
        | Some a ->
          Some
            ( a
            , let b = a / 10 in
              if b = 0 then None else Some b ))
      (Some n)
    |> List.of_seq
  in
  if n > 0 && n mod 10 = 0 then List.append lst [ 0 ] else lst
;;

let lefts n =
  let ns = Seq.repeat n in
  let mods = Seq.iterate (fun n -> n * 10) 10 in
  Seq.map2 (fun n m -> n mod m) ns mods
  |> Seq.take_while (fun n' -> n' <> n)
  |> List.of_seq
  |> List.rev
  |> List.cons n
;;

let is_trunc_prime = function
  | n when n < 11 -> false
  | n ->
    let r = rights n in
    let l = lefts n in
    List.for_all Util.is_prime r && List.for_all Util.is_prime l
;;

let _ =
  let cnt = ref 0 in
  Util.gen_primes 100_000_000
  |> List.filter is_trunc_prime
  |> List.take_while (fun _ ->
    if !cnt >= 11
    then false
    else (
      incr cnt;
      true))
  |> List.fold_left ( + ) 0
  |> Printf.printf "%d\n"
;;
