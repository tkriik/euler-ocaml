(* #require "zarith" *)
let rec fact: Z.t -> Z.t = function
  | n when Z.equal n Z.zero -> Z.one
  | n -> Z.mul n (fact (Z.sub n Z.one))

let total = Z.of_int 40
let rights = Z.of_int 20
let downs = Z.of_int 20
let ans =
  Z.div (fact total) (Z.mul (fact rights) (fact downs))
  |> Z.to_string
  |> print_endline
