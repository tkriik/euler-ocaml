let t n = n * (n + 1) / 2

module IntSet = Set.Make (struct
    type t = int

    let compare = compare
  end)

let tset = List.init 25 succ |> List.map t |> IntSet.of_list

let words =
  In_channel.with_open_text "p42.in" In_channel.input_all |> String.split_all ~sep:","
;;

let word_val s =
  let c_val c =
    let i = 1 + (int_of_char c - int_of_char 'A') in
    if 0 < i && i <= 26 then i else 0
  in
  String.fold_left (fun acc c -> acc + c_val c) 0 s
;;

let is_triangle_word s = IntSet.mem (word_val s) tset
let _ = List.filter is_triangle_word words |> List.length |> Printf.printf "%d\n"
