let rec compress = function
  | [] -> []
  | 1 :: 1 :: xs -> 2 :: compress xs
  | 1 :: 2 :: 2 :: xs -> 5 :: compress xs
  | [ 5; 5 ] -> [ 10 ]
  | [ 2; 2; 2; 2; 2 ] -> [ 10 ]
  | [ 10 ] -> [ 10 ]
  | _ -> invalid_arg "shouldn't happen"
;;

let ps = List.init 10 (fun _ -> 1)
let ans = Seq.iterate compress ps |> Seq.take 10 |> List.of_seq
