open Char.Ascii
open Printf

let rec word = function
  | 1 -> "one"
  | 2 -> "two"
  | 3 -> "three"
  | 4 -> "four"
  | 5 -> "five"
  | 6 -> "six"
  | 7 -> "seven"
  | 8 -> "eight"
  | 9 -> "nine"
  | 10 -> "ten"
  | 11 -> "eleven"
  | 12 -> "twelve"
  | 13 -> "thirteen"
  | 14 -> "fourteen"
  | 15 -> "fifteen"
  | 16 -> "sixteen"
  | 17 -> "seventeen"
  | 18 -> "eighteen"
  | 19 -> "nineteen"
  | 20 -> "twenty"
  | 30 -> "thirty"
  | 40 -> "forty"
  | 50 -> "fifty"
  | 60 -> "sixty"
  | 70 -> "seventy"
  | 80 -> "eighty"
  | 90 -> "ninety"
  | n when 20 < n && n < 100 ->
      let tens = n - (n mod 10) in
      let sub_n = n mod 10 in
      sprintf "%s-%s" (word tens) (word sub_n)
  | n when 100 <= n && n < 1000 ->
      let hundreds = n / 100 in
      let sub_n = n - (hundreds * 100) in
      if sub_n = 0 then
        sprintf "%s hundred" (word hundreds)
      else
        sprintf "%s hundred and %s" (word hundreds) (word sub_n)
  | 1000 -> "one thousand"
  | _ -> "<TODO>"

let count_letters s =
  String.fold_left (fun acc c -> if is_letter c then acc + 1 else acc) 0 s

let ans =
  List.init 1000 succ
  |> List.map word
  |> List.map count_letters
  |> List.fold_left (( + )) 0
