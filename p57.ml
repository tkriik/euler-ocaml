let expansions cnt =
  let rec aux = function
    | 0 -> Q.zero
    | n when 0 < n -> Q.div Q.one (Q.add (Q.of_int 2) (aux (n - 1)))
    | _ -> invalid_arg "count must be positive"
  in
  Q.add Q.one (aux cnt)
;;

let num_has_more_digits (q : Q.t) =
  let numstr, denstr = Z.to_string q.num, Z.to_string q.den in
  String.length numstr > String.length denstr
;;

let _ =
  List.init 1000 succ
  |> List.map expansions
  |> List.filter num_has_more_digits
  |> List.length
  |> Printf.printf "%d\n"
;;
