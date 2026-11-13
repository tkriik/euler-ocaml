let dotted pos s =
  if pos < 0 || pos > String.length s then invalid_arg "position out of bounds";
  String.sub s 0 pos ^ "." ^ String.sub s pos (String.length s - pos)

let ans =
  In_channel.with_open_text "p13.in" In_channel.input_lines
  |> map (fun l -> float_of_string (dotted 20 l))
  |> fold_left (( +. )) 0.
  |> string_of_float
  |> (fun s -> String.sub s 0 10)
