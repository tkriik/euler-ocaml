let digits n d =
  Seq.unfold (fun r -> if r = 0 then None else Some (10 * r / d, 10 * r mod d)) (n mod d)
;;

let int_list_to_string lst = List.map string_of_int lst |> String.concat ""

let seq_chunks ?(len = 1) ?(pos = 0) seq =
  Seq.drop pos seq
  |> Seq.unfold (fun seq ->
    let chunk = Seq.take len seq in
    if Seq.is_empty chunk
    then None
    else Some (chunk |> List.of_seq |> int_list_to_string, Seq.drop len seq))
  |> Seq.filter (fun chunk -> String.length chunk = len)
;;

let does_repeat seq =
  match seq () with
  | Seq.Nil -> false
  | Seq.Cons (x, tl) when not (Seq.is_empty tl) -> Seq.for_all (fun y -> y = x) tl
  | _ -> false
;;

exception Cycle of int

let cycle_len d =
  try
    let cutoff = d * 2 in
    let frac = Seq.take cutoff (digits 1 d) in
    if Seq.length frac < cutoff then raise (Cycle 0);
    for len = 1 to cutoff / 2 do
      for pos = 0 to len - 1 do
        let is_cycle = frac |> seq_chunks ~len ~pos |> does_repeat in
        if is_cycle then raise (Cycle len)
      done
    done;
    0
  with
  | Cycle len ->
    Printf.printf "l/%d\tlen = %d\n%!" d len;
    len
;;

let ans =
  List.init 999 succ
  |> List.rev
  |> List.mapi (fun i q -> i + 1, cycle_len q)
  |> List.fold_left
       (fun (nmax, lenmax) (n, len) -> if lenmax < len then n, len else nmax, lenmax)
       (0, 0)
  |> fun (n, len) -> Printf.printf "max: n = %d\tlen = %d\n" n len
;;
