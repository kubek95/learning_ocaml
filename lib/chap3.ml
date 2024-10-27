(*2nd Exercise: product*)
let rec product = function
| [] -> 1
| x :: xs -> x * product xs

let product_tl lst =
  let rec aux acc = function
  | [] -> acc
  | x :: xs -> aux (x*acc) xs
in
aux 1 lst

(*3rd exercise: concat*)
let rec concat = function
| [] -> ""
| x :: xs -> x ^ concat xs