let num_digits z = String.length (Z.to_string z)
let fibs = Seq.unfold (fun (a, b) -> Some (a, (b, Z.add a b))) (Z.one, Z.one)

let () =
  Seq.find_index (fun z -> num_digits z = 1000) fibs
  |> Option.get
  |> ( + ) 1
  |> Printf.printf "%d\n"
;;
