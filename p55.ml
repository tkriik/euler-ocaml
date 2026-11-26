let rev (z : Z.t) : Z.t =
  let z10 = Z.of_int 10 in
  let rec aux acc z' =
    let d = Z.rem z' z10 in
    let acc' = Z.add (Z.mul acc z10) d in
    if Z.lt z' z10 then acc' else aux acc' (Z.div z' z10)
  in
  aux Z.zero z
;;

let is_lychrel n =
  let max_iters = 50 in
  let rec aux iters = function
    | _ when max_iters <= iters -> true
    | z when 0 < iters && Z.equal z (rev z) -> false
    | z -> aux (iters + 1) (Z.add z (rev z))
  in
  aux 0 (Z.of_int n)
;;

let _ =
  List.init 10_000 Fun.id |> List.filter is_lychrel |> List.length |> Printf.printf "%d\n"
;;
