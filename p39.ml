let _ =
  let max_p = ref (-1) in
  let max_sols = ref 0 in
  for p = 1000 downto 12 do
    let sols = ref 0 in
    for a = 3 to p / 3 do
      for b = a + 1 to p / 2 do
        for c = b + 1 to p / 2 do
          if (a * a) + (b * b) = c * c && a + b + c = p then incr sols
        done
      done
    done;
    if !max_sols < !sols
    then (
      max_p := p;
      max_sols := !sols)
  done;
  Printf.printf "p = %d -> %d solutions\n" !max_p !max_sols
;;
