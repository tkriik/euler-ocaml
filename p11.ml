let grid =
  In_channel.with_open_text "p11.in" In_channel.input_lines
  |> List.map (String.split_on_char ' ')
  |> List.map (List.map int_of_string)
  |> List.map Array.of_list
  |> Array.of_list

let (w, h) = (Array.length grid.(0), Array.length grid)

let get = function
  | x, _ when x < 0 || w <= x -> 1
  | _, y when y < 0 || h <= y -> 1
  | x, y -> grid.(y).(x)

let ans =
  let max_prod = ref 0 in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      max_prod := max !max_prod @@ List.fold_left max 0 [
        get (x, y) * get (x + 1, y + 0) * get (x + 2, y + 0) * get (x + 3, y + 0);
        get (x, y) * get (x + 0, y + 1) * get (x + 0, y + 2) * get (x + 0, y + 3);
        get (x, y) * get (x + 1, y + 1) * get (x + 2, y + 2) * get (x + 3, y + 3);
        get (x, y) * get (x - 1, y + 1) * get (x - 2, y + 2) * get (x - 3, y + 3)
      ]
    done
  done;
  !max_prod
