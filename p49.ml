let rec dhash n = if n = 0 then Int.hash 0 else Int.hash (n mod 10) * dhash (n / 10)
let ntab = Hashtbl.create 1024

let register n =
  let k = dhash n in
  match Hashtbl.find_opt ntab k with
  | None -> Hashtbl.add ntab k [ n ]
  | Some ns -> Hashtbl.replace ntab k (List.sort compare (n :: ns))
;;

let primes =
  let ps = List.filter (fun n -> 1000 <= n) (Util.gen_primes 10_000) in
  List.iter (fun p -> register p) ps;
  ps
;;

let rec power_set = function
  | [] -> [ [] ]
  | x :: xs ->
    let ps = power_set xs in
    ps @ List.map (fun subset -> x :: subset) ps
;;

let diffs ns =
  let len = List.length ns - 1 in
  List.map2 (fun x y -> x - y) (List.tl ns) (List.take len ns)
;;

let does_repeat = function
  | [] | [ _ ] -> false
  | n :: ns -> List.for_all (fun n' -> n = n') ns
;;

let is_permseq ns = does_repeat (diffs ns)

exception Found of int list

let search ns =
  let pns = List.filter (fun lst -> 3 = List.length lst) (power_set ns) in
  List.iter (fun ns -> if is_permseq ns then raise (Found ns)) pns
;;

let _ =
  (try Hashtbl.iter (fun _ ns -> search ns) ntab with
   | Found ns ->
     print_endline (List.fold_left (fun s n -> s ^ string_of_int n) "" ns);
     exit 0);
  print_endline "not found";
  exit 1
;;

(*
open Printf

let dbg () =
  Hashtbl.iter
    (fun k ns ->
       printf "%d -> " k;
       List.iter (fun n -> printf "%d " n) ns;
       print_endline "";
       search ns)
    ntab
;;
*)
