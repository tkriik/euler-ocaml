#require "zarith"

let pow a b =
  List.init b (fun _ -> Z.of_int a) |> List.fold_left (fun acc n -> Z.mul acc n) Z.one
;;

let distincts (a_lo, a_hi) (b_lo, b_hi) =
  let ns = Hashtbl.create 1024 in
  for a = a_lo to a_hi do
    for b = b_lo to b_hi do
      Hashtbl.replace ns (pow a b) true
    done
  done;
  Hashtbl.to_seq_keys ns |> Seq.length
;;

let answer = distincts (2, 100) (2, 100)
