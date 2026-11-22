let coins = [| 0; 1; 2; 5; 10; 20; 50; 100; 200 |]
let target = 200
let ways = Array.make (target + 1) 0
let () = ways.(0) <- 1

let _ =
  for i = 1 to 8 do
    for j = coins.(i) to target do
      ways.(j) <- ways.(j) + ways.(j - coins.(i))
    done
  done;
  Printf.printf "%d\n" ways.(target)
;;
