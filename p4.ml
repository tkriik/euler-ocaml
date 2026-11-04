let reverse s =
  let len = String.length s in
  String.init len (fun n -> String.get s (len - n - 1))

let is_palindrome_number n = (string_of_int n) = (reverse (string_of_int n))
let max_int_in lst = List.fold_left max 0 lst

let i = ref 999
let max_palindrome = ref 0
let () =
  while !i > 0 do
    let res =
      List.init !i succ |>
      List.map (( * ) !i) |>
      List.filter is_palindrome_number |>
      max_int_in in
    max_palindrome := max !max_palindrome res;
    decr i
  done
let ans = !max_palindrome
