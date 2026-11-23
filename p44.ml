let penta n = n * ((3 * n) - 1) / 2
let dim = 10_000
let pentas = Hashtbl.create dim
let _ = List.init dim succ |> List.iter (fun n -> Hashtbl.add pentas (penta n) n)

let is_penta_sum x y =
  assert (x <= dim && y <= dim);
  Hashtbl.mem pentas (penta x + penta y)
;;

let is_penta_diff x y =
  assert (x <= dim && y <= dim);
  Hashtbl.mem pentas (penta x - penta y)
;;

let _ =
  let exception Found of int * int in
  (try
     for y = 1 to dim do
       for x = y + 1 to dim do
         if is_penta_sum x y && is_penta_diff x y then raise (Found (x, y))
       done
     done
   with
   | Found (x, y) ->
     Printf.printf "%d\n" (penta x - penta y);
     exit 0);
  Printf.eprintf "not found";
  exit 1
;;
