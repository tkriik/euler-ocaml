open List
open Printf

let digits = function
  | n when abs n < 10 -> [ n ]
  | n ->
    let rec aux n' = if n' = 0 then [] else (n' mod 10) :: aux (n' / 10) in
    rev (aux n)
;;

let rec fac = function
  | 0 -> 1
  | n when 0 < n -> n * fac (n - 1)
  | _ -> invalid_arg "must be positive"
;;

let digit_fac_sum n = digits n |> map fac |> fold_left ( + ) 0

let _ =
  init 1_000_000 (fun n -> n + 3)
  |> filter (fun n -> digit_fac_sum n = n)
  |> fold_left ( + ) 0
  |> printf "%d\n"
;;
