#load "zarith.cma"

let rec fact z = if z = Z.zero then Z.one else Z.mul z (fact (Z.sub z (Z.of_int 1)))

let ans =
  Z.of_int 100
  |> fact
  |> Z.to_string
  |> String.to_seq
  |> List.of_seq
  |> List.map Char.Ascii.digit_to_int
  |> List.fold_left (( + )) 0
