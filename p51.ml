let is_prime, next_prime, digits, int_log10 =
  Util.(is_prime, next_prime, digits, int_log10)
;;

let rec power_set = function
  | [] -> [ [] ]
  | x :: xs ->
    let ps = power_set xs in
    ps @ List.map (fun subset -> x :: subset) ps
;;

let free_indices len =
  let is_valid idxs =
    let sub_len = List.length idxs in
    0 < sub_len && sub_len < len
  in
  List.init len (fun i -> i) |> power_set |> List.filter is_valid
;;

type term =
  | Digit of int
  | FreeLeading
  | Free

let term_variants n =
  let ds = Array.of_list (digits n) in
  let len = Array.length ds in
  let frees = free_indices len in
  let rec indices_to_terms idxs =
    Array.mapi
      (fun i d ->
         if List.mem i idxs then if i = 0 then FreeLeading else Free else Digit d)
      ds
  in
  List.map indices_to_terms frees
;;

exception Found of int * int list

let search_variants n expect_cnt vs =
  let ds = List.init 10 (fun n -> n) in
  let apply d =
    Array.map
      (function
        | Digit d' -> d'
        | FreeLeading -> d
        | Free -> d)
      vs
  in
  let array_to_int a = Array.fold_left (fun acc d -> (acc * 10) + d) 0 a in
  let ps = List.map apply ds |> List.map array_to_int |> List.filter is_prime in
  (* remove smaller first prime due to zero at free leading digit *)
  let fixed =
    match ps with
    | [] | [ _ ] -> ps
    | x :: y :: xs as lst -> if int_log10 x != int_log10 y then y :: xs else lst
  in
  if expect_cnt <= List.length fixed then raise (Found (n, ps))
;;

let search expect_cnt n = term_variants n |> List.iter (search_variants n expect_cnt)

let _ =
  let expected_primes = 8 in
  try Seq.iterate next_prime 2 |> Seq.iter (search expected_primes) with
  | Found (_, ps) -> Printf.printf "%d\n" (List.hd ps)
;;
