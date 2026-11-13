let gen_primes count =
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

let ans = fold_left (( + )) 0 (gen_primes 1_999_999)
