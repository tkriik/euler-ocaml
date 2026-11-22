module T = Domainslib.Task

let len1, off1 = 8, 0
let len2, off2 = 7, off1 + len1
let len5, off5 = 6, off2 + len2
let len10, off10 = 5, off5 + len5
let len20, off20 = 4, off10 + len10
let len50, off50 = 3, off20 + len20
let len100, off100 = 2, off50 + len50
let len200, off200 = 1, off100 + len100

let pack (p200, p100, p50, p20, p10, p5, p2, p1) =
  (p200 lsl off200)
  lor (p100 lsl off100)
  lor (p50 lsl off50)
  lor (p20 lsl off20)
  lor (p10 lsl off10)
  lor (p5 lsl off5)
  lor (p2 lsl off2)
  lor (p1 lsl off1)
;;

let lenmask len = (1 lsl len) - 1

let prodsum n =
  (1 * ((n lsr off1) land lenmask len1))
  + (2 * ((n lsr off2) land lenmask len2))
  + (5 * ((n lsr off5) land lenmask len5))
  + (10 * ((n lsr off10) land lenmask len10))
  + (20 * ((n lsr off20) land lenmask len20))
  + (50 * ((n lsr off50) land lenmask len50))
  + (100 * ((n lsr off100) land lenmask len100))
  + (200 * ((n lsr off200) land lenmask len200))
;;

let compute from_n to_n =
  let cnt = ref 0 in
  for n = from_n downto to_n do
    if prodsum n = 200 then incr cnt
  done;
  !cnt
;;

let compute_all pool num_domains init =
  if init < 10_000_000
  then compute init 0
  else (
    let chunk = (init + 1) / num_domains in
    let ps = ref [] in
    for i = 0 to num_domains - 1 do
      let from_n = init - (i * chunk) in
      let to_n = if i < num_domains then from_n - (chunk - 1) else 0 in
      let p = T.async pool (fun _ -> compute from_n to_n) in
      ps := p :: !ps
    done;
    List.map (fun p -> T.await pool p) !ps |> List.fold_left ( + ) 0)
;;

let main () =
  let num_domains = 15 in
  let pool = T.setup_pool ~num_domains () in
  (* start from highest *)
  let init = pack (1, 0, 0, 0, 0, 0, 0, 0) in
  let res = T.run pool (fun _ -> compute_all pool num_domains init) in
  T.teardown_pool pool;
  Printf.printf "res: %d\n" res
;;

let _ = main ()
