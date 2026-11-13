open List

let sq x = x * x

let is_pyth (a, b, c) = a < b && b < c && (sq a) + (sq b) = (sq c)

let gen_pyth = function
  | c when c < 3 -> []
  | c ->
      let abcs = ref [] in
      for i = c downto 3 do
        for j = i - 1 downto 2 do
          for k = j - 1 downto 1 do
            let abc = (k, j, i) in
            if is_pyth(abc) then abcs := (k, j, i) :: !abcs
          done
        done
      done;
      !abcs

let abc_sum (a, b, c) = a + b + c

let is_abc_1000 abc = abc_sum abc = 1000

let ans = match gen_pyth 500 |> filter is_abc_1000 with
  | [(a, b, c)] -> Some (a * b * c)
  | _ -> None
