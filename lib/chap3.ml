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

(*6th exercise: library puzzle*)
let get_last lst =
  lst |> List.rev |> List.hd

let any_zero lst =
  let is_zero x = x = 0 in
  List.exists is_zero lst

(*7th exercise: take drop*)
let rec take n = function
  | [] -> []
  | x :: xs ->
    if n=0 then [] else x :: take (n-1) xs

let rec drop n = function
  | [] -> []
  | x :: xs ->
    if n=0 then x :: xs else drop (n-1) xs

(*8th exercise: take tail recursive*)
let take_tr n lst =
  let rec aux acc n lst =
    if n=0 then acc else match lst with
    | [] -> acc
    | x :: xs -> aux (x :: acc) (n-1) xs
  in
  lst |> aux [] n |> List.rev
(*drop is already tail recursive*)

(*9th exercise: unimodal*)
let is_unimodal lst =
  let rec is_mono_desc = function
  | [] | [_] -> true
  | x :: x' :: xs ->
    if x >= x' then is_mono_desc (x' :: xs) else false
  in
  let rec is_asc_then_desc = function
  | [] | [_] -> true
  | x :: x' :: xs ->
    if x < x' then is_asc_then_desc (x' :: xs) else is_mono_desc (x :: x' :: xs)
  in
  is_asc_then_desc lst
