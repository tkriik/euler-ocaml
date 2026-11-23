(*
let rec dhash n = if n = 0 then 1 * Int.hash 0 else Int.hash (n mod 10) * dhash (n / 10)
let dh09 = dhash 123456789 * Int.hash 0
let is_panint09 n = dhash n = dh09

let has_prop n =
  let s = string_of_int n in
  let slice3 pos = int_of_string (String.sub s pos 3) in
  slice3 1 mod 2 = 0
  && slice3 2 mod 3 = 0
  && slice3 3 mod 5 = 0
  && slice3 4 mod 7 = 0
  && slice3 5 mod 11 = 0
  && slice3 6 mod 13 = 0
  && slice3 7 mod 17 = 0
;;

let _ =
  let sum = ref 0 in
  for n = 1_000_000_000 to 9_000_000_000 do
    if is_panint09 n && has_prop n then sum := !sum + n
  done;
  Printf.printf "%d\n" !sum
;;
*)

(*
open Printf

type 'a tree =
  | Root of 'a * 'a * 'a * 'a * 'a tree list
  | Branch of 'a * 'a * 'a * 'a tree list

let gen_trees () =
  let rec aux prefix (x, y) div =
    let subtrees = ref [] in
    for z = 0 to 9 do
      let n = (x * 100) + (y * 10) + z in
      if (not (List.mem z prefix)) && n mod div = 0
      then (
        let next_prefix = z :: prefix in
        let subtree =
          if List.length next_prefix > 10 then invalid_arg "prefix too long";
          if List.length next_prefix = 10
          then Branch (x, y, z, [])
          else Branch (x, y, z, aux next_prefix (y, z) (Util.next_prime div))
        in
        subtrees := subtree :: !subtrees)
    done;
    !subtrees
  in
  let roots = ref [] in
  let div = 2 in
  for n = 0 to 9999 do
    let i, xyz = n / 1000, n mod 1000 in
    if xyz mod div = 0
    then (
      let[@warning "-8"] [ x; y; z ] =
        xyz
        |> sprintf "%03d"
        |> String.to_seq
        |> Seq.map Char.Ascii.digit_to_int
        |> List.of_seq
      in
      roots := Root (i, x, y, z, []) :: !roots)
  done;
  List.map
    (fun[@warning "-8"] (Root (i, x, y, z, _)) ->
       Root (i, x, y, z, aux [ z; y; x; i ] (y, z) (Util.next_prime div)))
    !roots
;;

let[@warning "-8"] sum_deeps (Root (i, x, y, z, subtrees)) =
  let sum = ref 0 in
  let rec aux prefix (Branch (_, _, z, subtrees)) =
    let depth = List.length prefix in
    if depth = 9
    then (
      List.iter print_int prefix;
      print_endline "";
      let subsum =
        List.rev (z :: prefix) |> List.fold_left (fun acc n -> (acc * 10) + n) 0
      in
      sum := !sum + subsum);
    List.iter (aux (z :: prefix)) subtrees
  in
  List.iter (aux [ z; y; x; i ]) subtrees;
  !sum
;;

let _ = gen_trees () |> List.fold_left (fun acc t -> acc + sum_deeps t) 0 |> printf "%d\n"
*)

type branch =
  { a : int
  ; b : int
  ; c : int
  ; prefix : int list
  ; branches : branch list
  }

type root =
  { i : int
  ; branches : branch list
  }

let int_of_prefix p = List.rev p |> List.fold_left (fun acc d -> (acc * 10) + d) 0
let is_digit n = 0 <= n && n < 10
let is_prime, next_prime = Util.(is_prime, next_prime)

let gen_root i =
  let rec gen_branch ~div ~prefix a b c =
    assert (is_prime div);
    List.iter (fun n -> assert (is_digit n)) prefix;
    List.iter (fun n -> assert (is_digit n)) [ a; b; c ];
    let branches = ref [] in
    for d = 0 to 9 do
      if not (List.mem d prefix)
      then (
        let bcd = (b * 100) + (c * 10) + d in
        if bcd mod div = 0
        then (
          let next_prefix = d :: prefix in
          let branch = gen_branch ~div:(next_prime div) ~prefix:next_prefix b c d in
          branches := branch :: !branches))
    done;
    { a; b; c; prefix; branches = !branches }
  in
  let div = 2 in
  let branches = ref [] in
  for a = 0 to 9 do
    for b = 0 to 9 do
      for c = 0 to 9 do
        let abc = (a * 100) + (b * 10) + c in
        if abc mod div = 0 && i <> a && i <> b && i <> c && a <> b && b <> c && a <> c
        then (
          let branch = gen_branch ~div:(next_prime div) ~prefix:[ c; b; a; i ] a b c in
          branches := branch :: !branches)
      done
    done
  done;
  { i; branches = !branches }
;;

let sum_pan09s { branches; _ } =
  let sum = ref 0 in
  let rec aux depth { prefix; branches; _ } =
    if branches = [] && depth = 7 then sum := !sum + int_of_prefix prefix;
    List.iter (aux (depth + 1)) branches
  in
  List.iter (aux 1) branches;
  !sum
;;

let _ =
  List.init 10 (fun i -> i)
  |> List.map (fun i -> sum_pan09s (gen_root i))
  |> List.fold_left ( + ) 0
  |> Printf.printf "%d\n"
;;
