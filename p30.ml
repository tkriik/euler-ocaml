let int_to_digits n =
  let rec f = function
    | n when -10 < n && n < 10 -> [ n ]
    | n -> (n mod 10) :: f (n / 10)
  in
  List.rev (f n)
;;

let pow n exp = List.init exp (fun _ -> n) |> List.fold_left ( * ) 1

let is_digit_power exp n =
  let m = int_to_digits n |> List.fold_left (fun sum d -> sum + pow d exp) 0 in
  n <> 1 && n = m
;;

let answer =
  List.init 1_000_000 succ |> List.filter (is_digit_power 5) |> List.fold_left ( + ) 0
;;

let () = Printf.printf "%d\n" answer
