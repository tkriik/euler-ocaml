open List
let square n = n * n
let sum xs = fold_left (fun x y -> x + y) 0 xs
let nats n = init n succ
let sum_of_squares n =  sum (map square (nats n))
let square_of_sum n = square (sum (nats n))
let ans = abs((sum_of_squares 100) - (square_of_sum 100))
