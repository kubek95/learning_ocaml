(*Exercise: repeat*)
let rec repeat f x n = if n = 0 then x else repeat f (f x) (n - 1)

(*Exercise: product*)
let product_left lst = List.fold_left ( *. ) 1. lst
let product_right lst = List.fold_right ( *. ) lst 1.

(*Exercise: sum_cube_odd/pipline*)
let rec ( -- ) x y = if x <= y then x :: (x + 1 -- y) else []

let sum_cube_odd n =
  0 -- n
  |> List.filter (fun x -> x mod 2 != 0)
  |> List.map (fun x -> x * x * x)
  |> List.fold_left ( + ) 0

(*Exercise: exists*)
let rec exists_rec p = function
  | [] -> false
  | x :: xs -> if p x then true else exists_rec p xs

let exists_fold p = List.fold_left (fun acc x -> acc || p x) false
let exists_lib f = List.exists f

(*Exercise: account balance*)
let rec balance_rec balance = function
  | [] -> balance
  | x :: xs -> balance_rec (balance -. x) xs

let balance_left balance = List.fold_left ( -. ) balance

let balance_right balance debits =
  List.fold_right (fun deb bal -> bal -. deb) debits balance

(*Exercise: library uncurried*)
let uncurried_nth (lst, n) = List.nth lst n
let uncurried_append (lst1, lst2) = List.append lst1 lst2
let uncurried_compare (ch1, ch2) = Char.compare ch1 ch2
let uncurried_max (x, y) = Stdlib.max x y

(*Exercise: map composition*)
let compose f g x = f (g x)
let map_composition f g = List.map (compose f g)

(*Exercise: more list fun*)
let greater_than_three = List.filter (fun x -> x > 3)
let add_one = List.map (fun x -> x +. 1.)

let concat_with_separator sep = function
  | [] -> ""
  | x :: xs -> List.fold_left (fun acc el -> acc ^ sep ^ el) x xs

(*Exercise: association list keys*)
let keys lst =
  List.sort_uniq (fun (k1, _) (k2, _) -> compare k1 k2) lst
  |> List.map (fun (k, _) -> k)

(*Exercise: valid matrix*)
let is_valid_matrix matrix =
  let rec const_list = function
    | [] | [ _ ] -> true
    | x :: x' :: xs -> if x = x' then const_list (x' :: xs) else false
  in
  match List.map List.length matrix with [] -> false | xs -> const_list xs

(*Exercise: row vector add*)
let add_row_vectors vec1 vec2 = List.map2 ( + ) vec1 vec2

(*Exercise: matrix add*)
let add_matrices = List.map2 add_row_vectors
