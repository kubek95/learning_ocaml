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

(*4th exercise: patterns*)
let first_is_bigred = function
| x :: _ when x = "bigred" -> true
| _ -> false

let two_or_four_elements = function
| _ :: _ :: []
| _ :: _ :: _ :: _ :: [] -> true
| _ -> false

let two_first_equal = function
| x :: y :: _ when x=y -> true
| _ -> false

(*5th exercise: library*)
let fifth_element (list: int list): int =
  match List.nth_opt list 4 with
  | None -> 0
  | Some x -> x

let sort_descending (list : int list) : int list =
  List.rev (List.sort Stdlib.compare list)