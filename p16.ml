(* #require "zarith" *)
let ans =
  Z.of_string "2"
  |> (fun n -> Z.pow n 1000)
  |> Z.to_string
  |> String.to_seq
  |> List.of_seq
  |> List.map Char.Ascii.digit_to_int
  |> List.fold_left (( + )) 0
