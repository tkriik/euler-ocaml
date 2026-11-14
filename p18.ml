let t =
  In_channel.with_open_text "p18.in" In_channel.input_lines
  |> List.map (String.split_on_char ' ')
  |> List.map (List.map int_of_string)
  |> List.map Array.of_list
  |> Array.of_list

let h = Array.length t

let x =
  for r = h - 1 downto 1 do
    for c = 1 to Array.length t.(r) - 1 do
      let max_n = max t.(r).(c - 1) t.(r).(c) in
      t.(r - 1).(c - 1) <- (fun x -> x + max_n) t.(r - 1).(c - 1)
    done
  done;
  t.(0).(0)
