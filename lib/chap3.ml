(*2nd Exercise: product*)
let rec product = function [] -> 1 | x :: xs -> x * product xs

let product_tl lst =
  let rec aux acc = function [] -> acc | x :: xs -> aux (x * acc) xs in
  aux 1 lst

(*3rd exercise: concat*)
let rec concat = function [] -> "" | x :: xs -> x ^ concat xs

(*4th exercise: patterns*)
let first_is_bigred = function x :: _ when x = "bigred" -> true | _ -> false

let two_or_four_elements = function
  | [ _; _ ] | [ _; _; _; _ ] -> true
  | _ -> false

let two_first_equal = function x :: y :: _ when x = y -> true | _ -> false

(*5th exercise: library*)
let fifth_element (list : int list) : int =
  match List.nth_opt list 4 with None -> 0 | Some x -> x

let sort_descending (list : int list) : int list =
  List.rev (List.sort Stdlib.compare list)

(*6th exercise: library puzzle*)
let get_last lst = lst |> List.rev |> List.hd

let any_zero lst =
  let is_zero x = x = 0 in
  List.exists is_zero lst

(*7th exercise: take drop*)
let rec take n = function
  | [] -> []
  | x :: xs -> if n = 0 then [] else x :: take (n - 1) xs

let rec drop n = function
  | [] -> []
  | x :: xs -> if n = 0 then x :: xs else drop (n - 1) xs

(*8th exercise: take tail recursive*)
let take_tr n lst =
  let rec aux acc n lst =
    if n = 0 then acc
    else match lst with [] -> acc | x :: xs -> aux (x :: acc) (n - 1) xs
  in
  lst |> aux [] n |> List.rev
(*drop is already tail recursive*)

(*9th exercise: unimodal*)
let is_unimodal lst =
  let rec is_mono_desc = function
    | [] | [ _ ] -> true
    | x :: x' :: xs -> if x >= x' then is_mono_desc (x' :: xs) else false
  in
  let rec is_asc_then_desc = function
    | [] | [ _ ] -> true
    | x :: x' :: xs ->
        if x < x' then is_asc_then_desc (x' :: xs)
        else is_mono_desc (x :: x' :: xs)
  in
  is_asc_then_desc lst

(*10th exercise: powerset*)
let rec powerset = function
  | [] -> [ [] ]
  | x :: xs ->
      let p = powerset xs in
      List.map (fun l -> x :: l) p @ p

(*11th exercise: print int list rec*)
let rec print_int_list = function
  | [] -> ()
  | x :: xs ->
      let () = x |> string_of_int |> print_endline in
      print_int_list xs

(*12th exercise: print int list iter*)
let print_int_list' lst =
  let print_int x = x |> string_of_int |> print_endline in
  List.iter print_int lst

(*13th exercise: student*)
type student = { first_name : string; last_name : string; gpa : float }

let john_doe = { first_name = "John"; last_name = "Doe"; gpa = 3.5 }

let get_student_name = function
  | { first_name; last_name; _ } -> (first_name, last_name)

let create_student name surname gpa =
  { first_name = name; last_name = surname; gpa }

(*14th exercise: pokerecord*)
type poketype = Normal | Fire | Water
type pokemon = { name : string; hp : int; ptype : poketype }

let charizard = { name = "Charizard"; hp = 78; ptype = Fire }
let squirtle = { name = "Squirtle"; hp = 44; ptype = Water }

(*15th exercise: safe hd and tl*)
let safe_hd = function [] -> None | x :: _ -> Some x
let safe_tl = function [] -> None | _ :: xs -> Some xs

(*16th exercise: pokefun*)
let max_hp lst =
  let rec aux max = function
    | [] -> Some max
    | x :: xs -> if x.hp > max.hp then aux x xs else aux max xs
  in
  match lst with [] -> None | x :: xs -> aux x xs

(*17th exercise: date before*)
type date = int * int * int

let is_before (date1 : date) (date2 : date) : bool =
  match date1 with
  | y1, m1, d1 -> (
      match date2 with
      | y2, m2, d2 ->
          y1 < y2 || (y1 = y2 && m1 < m2) || (y1 = y2 && m1 = m2 && d1 < d2))

(*18th exercise: earliest date*)
let rec earliest = function
  | [] -> None
  | x :: xs -> (
      match earliest xs with
      | None -> Some x
      | Some z -> if is_before x z then Some x else Some z)

(*19th exercise: assoc list*)
let insert k v lst = (k, v) :: lst

let rec lookup key = function
  | [] -> None
  | (k, v) :: xs -> if k = key then Some v else lookup key xs

let assoc_list = insert 1 "one" (insert 2 "two" (insert 3 "three" []))
let key_two = lookup 2 assoc_list
let key_four = lookup 4 assoc_list

(*20th exercise: cards*)
type suit = Clubs | Diamonds | Hearts | Spades

type rank =
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type card = { rank : rank; suit : suit }

let ace_of_clubs = { rank = Ace; suit = Spades }
let queen_of_hearts = { rank = Queen; suit = Hearts }
let seven_of_spades = { rank = Seven; suit = Spades }

(*22nd exercise: quadrant*)
type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x : int) : sign =
  match x with 0 -> Zero | x -> if x < 0 then Neg else Pos

let quadrant (point : int * int) : quad option =
  let x, y = point in
  match (sign x, sign y) with
  | Pos, Pos -> Some I
  | Neg, Pos -> Some II
  | Neg, Neg -> Some III
  | Pos, Neg -> Some IV
  | _, _ -> None

(*23rd exercise: quadrant when*)
let quadrant_when = function
  | x, y when x > 0 && y > 0 -> Some I
  | x, y when x < 0 && y > 0 -> Some II
  | x, y when x < 0 && y < 0 -> Some III
  | x, y when x > 0 && y < 0 -> Some IV
  | _ -> None

(*24th exercise: depth*)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec depth = function
  | Leaf -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right)

(*25th exercise: shape*)
let rec same_shape tree1 tree2 =
  match (tree1, tree2) with
  | Leaf, Leaf -> true
  | Leaf, Node _ -> false
  | Node _, Leaf -> false
  | Node (_, l1, r1), Node (_, l2, r2) -> same_shape l1 l2 && same_shape r1 r2

(*26th exercise: list max exn*)
let rec list_max = function
  | [] -> raise (Failure "list max")
  | [ x ] -> x
  | x :: xs -> max x (list_max xs)

(*27th exercise: list max exn string*)
let list_max_string lst =
  try list_max lst |> string_of_int with Failure _ -> "empty"

(*28th exercise: is_bst*)

(*29th exercise: quadrant poly*)
let sign = function x when x < 0 -> `Neg | x when x = 0 -> `Zero | _ -> `Pos

let quadrant' (x, y) =
  match (sign x, sign y) with
  | `Pos, `Pos -> Some `I
  | `Neg, `Pos -> Some `II
  | `Neg, `Neg -> Some `III
  | `Pos, `Neg -> Some `IV
  | _ -> None

(*Chapter 4*)
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
