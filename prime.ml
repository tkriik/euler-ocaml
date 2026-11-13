let gen_list count =
  if count < 2 then []
  else begin
    let composite = Array.make (count + 1) false in
    let p = ref 2 in
    while !p <= count / !p do
      if not composite.(!p) then begin
        let multiple = ref (!p * !p) in
        while !multiple <= count do
          composite.(!multiple) <- true;
          multiple := !multiple + !p
        done
    end;
    incr p
    done;
    let result = ref [] in
    for n = count downto 2 do
      if not composite.(n) then result := n :: !result
    done;
    !result
  end

let gen_array count = gen_list count

let is_prime n =
  if n = 2 || n = 3 then true
  else if n < 2 || n mod 2 = 0 || n mod 3 = 0 then false
  else
    let rec test d =
      if d > n / d then true
      else if n mod d = 0 || n mod (d + 2) = 0 then false
      else test (d + 6)
    in
    test 5
