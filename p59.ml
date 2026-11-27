let decrypt (codes : int Array.t) (password : int Array.t) =
  let copied = Array.copy codes in
  let len = Array.length copied in
  let pw_len = Array.length password in
  for i = 0 to len - 1 do
    copied.(i) <- copied.(i) lxor password.(i mod pw_len)
  done;
  copied
;;

let codes_to_string codes = Array.to_seq codes |> Seq.map char_of_int |> String.of_seq

let has_english (codes : int Array.t) =
  let search_words =
    [ "the"; "be"; "to"; "of"; "and"; "a"; "in"; "that"; "have"; "i"; "it" ]
  in
  let words = String.split_on_char ' ' (codes_to_string codes) in
  let matches = ref 0 in
  List.iter (fun sw -> if List.mem sw words then incr matches) search_words;
  if !matches > 5 then true else false
;;

let input_codes : int Array.t =
  In_channel.with_open_text "p59.in" In_channel.input_all
  |> String.split_on_char ','
  |> List.map int_of_string
  |> Array.of_list
;;

let _ =
  for a = 97 to 122 do
    for b = 97 to 122 do
      for c = 97 to 122 do
        let password = [| a; b; c |] in
        let decrypted = decrypt input_codes password in
        if has_english decrypted
        then Array.fold_left ( + ) 0 decrypted |> Printf.printf "%d\n"
      done
    done
  done
;;
