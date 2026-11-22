let int_to_digits = function
  | n when abs n < 10 -> [ n ]
  | n ->
    let rec aux n = if n = 0 then [] else (n mod 10) :: aux (n / 10) in
    aux n
;;

let int_to_bits n =
  let rec aux n = if n < 2 then [ n ] else (n mod 2) :: aux (n / 2) in
  aux n
;;

let is_double_pal n =
  let a = int_to_digits n in
  let b = int_to_bits n in
  a = List.rev a && b = List.rev b
;;

let _ =
  List.init 1_000_000 succ
  |> List.filter is_double_pal
  |> List.fold_left ( + ) 0
  |> Printf.printf "%d\n"
;;
