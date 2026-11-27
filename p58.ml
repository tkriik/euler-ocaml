type needle =
  { num : int
  ; ring : int
  ; corner : int
  ; width : int
  ; prime_cnt : int
  ; total_cnt : int
  ; ratio : float
  }

let init =
  { num = 1; ring = 1; corner = 1; width = 1; prime_cnt = 0; total_cnt = 1; ratio = 0.0 }
;;

let next { num; ring; corner; width; prime_cnt; total_cnt; _ } =
  let num' = num + if ring = 1 then 2 else ring * 2 in
  let ring', corner' = if corner = 4 then ring + 1, 1 else ring, corner + 1 in
  let prime_cnt' = if Util.is_prime num' then prime_cnt + 1 else prime_cnt in
  let total_cnt' = total_cnt + 1 in
  { num = num'
  ; ring = ring'
  ; corner = corner'
  ; width = (if ring = ring' then width else width + 2)
  ; prime_cnt = prime_cnt'
  ; total_cnt = total_cnt'
  ; ratio = float_of_int prime_cnt' /. float_of_int total_cnt'
  }
;;

let () =
  Seq.iterate next init
  |> Seq.filter (fun { width; corner; ratio; _ } ->
    width >= 3 && corner = 1 && ratio < 0.1)
  |> Seq.take 1
  |> Seq.iter (fun { width; _ } -> Printf.printf "%d\n" width)
;;
