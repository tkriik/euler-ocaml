let rec routes = function
  | 1 -> 1
  | 2 -> 1 + routes 1
  | 5 -> 1 + routes 2 + routes 1
  | 10 -> 1 + routes 5 + routes 2 + routes 1
  | _ -> invalid_arg "unknown routes"
;;

(*
    5 ->
5
2+2+1
2+1+1+1
1+1+1+1+1
    10 ->
10
5+5
5+2+2+1
5+2+1+1+1
5+1+1+1+1+1
2+2+1+1+1+1+1+1
2+2+2+1+1+1+1
2+2+2+2+1+1
2+1+1+1+1+1+1+1+1
1+1+1+1+1+1+1+1+1+1
*)
