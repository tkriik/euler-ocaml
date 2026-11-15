open Char.Ascii

let names =
  In_channel.with_open_text "p22.in" In_channel.input_line
  |> Option.get
  |> String.replace_all ~sub:"\"" ~by:""
  |> String.split_on_char ',' |> List.sort String.compare

let score ?(pos = 1) s =
  let char_score c = 1 + (int_of_char c - int_of_char 'A') in
  pos * String.fold_left (fun acc c -> acc + char_score c) 0 s

let ans =
  names |> List.mapi (fun i s -> score s ~pos:(i + 1)) |> List.fold_left ( + ) 0

let () = Printf.printf "%d\n" ans
