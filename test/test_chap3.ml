open OUnit2
open Chap3

let make_product_test name product_version expected_output input =
  name >::(fun _ -> assert_equal expected_output (product_version input) ~printer:string_of_int)
let rec print_list = function
  | [] -> ""
  | x :: xs -> string_of_int x ^ " " ^ print_list xs

let product_tests = "recursion solution for exercise number 3" >::: [
  make_product_test "given empty list expect product to be 1" product 1 [];
  make_product_test "given one element expect product to be equal to given element" product 2 [2];
  make_product_test "given multiple elements expect product to return product of given values" product 3 [1;3];
]

let tail_rec_product_tests = "tail recursion solution for exercise number 3" >::: [
  make_product_test "given empty list expect product to be 1" product_tl 1 [];
  make_product_test "given one element expect product to be equal to given element" product_tl 2 [2];
  make_product_test "given multiple elements expect product to return product of given values" product_tl 3 [1;3];
]

let _ = run_test_tt_main product_tests
let _ = run_test_tt_main tail_rec_product_tests

let concat_tests = "tests for concatenation function" >::: [
  "empty list" >:: (fun _ -> assert_equal "" (concat []));
  "one element list" >:: (fun _ -> assert_equal "a" (concat ["a"]));
  "multiple elements list" >:: (fun _ -> assert_equal "asdf" (concat ["a";"s";"df"]))
]

let _ = run_test_tt_main concat_tests

let pattern_tests = "tests for bigred" >::: [
  "first element is bigred" >:: (fun _ -> assert_equal true (first_is_bigred ["bigred"]));
  "first element is not a bigred" >:: (fun _ -> assert_equal false (first_is_bigred ["smallgreen"]));
  "empty list" >:: (fun _ -> assert_equal false (first_is_bigred []));
]
let count_elements_tests = "tests for 2 or 4 element list" >::: [
  "empty list" >:: (fun _ -> assert_equal false (two_or_four_elements []));
  "one element" >:: (fun _ -> assert_equal false (two_or_four_elements [1]));
  "two elements" >:: (fun _ -> assert_equal true (two_or_four_elements [1;1]));
  "four elements" >:: (fun _ -> assert_equal true (two_or_four_elements [1;1;1;1]));
  "five elements" >:: (fun _ -> assert_equal false (two_or_four_elements [1;1;1;1;1]));
]
let two_fist_elements_tests = "tests for equality of first two elements" >::: [
  "empty list" >:: (fun _ -> assert_equal false (two_first_equal []));
  "one element" >:: (fun _ -> assert_equal false (two_first_equal [1]));
  "two equal elements" >:: (fun _ -> assert_equal true (two_first_equal [1;1;4]));
  "two unequal elements" >:: (fun _ -> assert_equal false (two_first_equal [1;3;5]));
]
let _ = run_test_tt_main pattern_tests
let _ = run_test_tt_main count_elements_tests
let _ = run_test_tt_main two_fist_elements_tests

let get_fifth_element_tests = "test for getting fith element out of a list" >::: [
  "empty list" >:: (fun _ -> assert_equal 0 (fifth_element []));
  "one element" >:: (fun _ -> assert_equal 0 (fifth_element [1]));
  "five elements" >:: (fun _ -> assert_equal 5 (fifth_element [1;2;3;4;5]));
]
let reverse_sort_tests = "test for sorting in descending order" >::: [
  "empty list" >:: (fun _ -> assert_equal [] (sort_descending []));
  "one element" >:: (fun _ -> assert_equal [1] (sort_descending [1]));
  "multiple elements" >:: (fun _ -> assert_equal [3;2;1] (sort_descending [1;2;3]));
]
let _ = run_test_tt_main get_fifth_element_tests
let _ = run_test_tt_main reverse_sort_tests

let get_last_element_tests = "test for getting last element out of a list" >::: [
  "one element" >:: (fun _ -> assert_equal 1 (get_last [1]));
  "multiple elements" >:: (fun _ -> assert_equal 5 (get_last [1;2;3;4;5]));
]
let is_zero_in_list_tests = "test for presence of zero in a list" >::: [
  "empty list" >:: (fun _ -> assert_equal false (any_zero []));
  "no zero in multiple element" >:: (fun _ -> assert_equal false (any_zero [1;3;4]));
  "zero in the list" >:: (fun _ -> assert_equal true (any_zero [1;2;0;3]));
]
let _ = run_test_tt_main get_last_element_tests
let _ = run_test_tt_main is_zero_in_list_tests

let take_tests = "test for getting sublist of a list" >::: [
  "empty list" >:: (fun _ -> assert_equal [] (take 2 []));
  "take zero elements" >:: (fun _ -> assert_equal [] (take 0 [1;2]));
  "take more than list size" >:: (fun _ -> assert_equal [1;2] (take 3 [1;2]));
  "take less than list size" >:: (fun _ -> assert_equal [1;2] (take 2 [1;2;3]));
]
let drop_tests = "test for droping n first elements of a list" >::: [
  "empty list" >:: (fun _ -> assert_equal [] (drop 1 []) ~printer:print_list);
  "drop zero elements" >:: (fun _ -> assert_equal [1;3] (drop 0 [1;3]) ~printer:print_list);
  "drop more than the size" >:: (fun _ -> assert_equal [] (drop 3 [1;2]) ~printer:print_list);
  "drop less than the size" >:: (fun _ -> assert_equal [3] (drop 2 [1;2;3]) ~printer:print_list);
]
let _ = run_test_tt_main take_tests
let _ = run_test_tt_main drop_tests

let take_tr_tests = "(tail rec ver) test for getting sublist of a list" >::: [
  "empty list" >:: (fun _ -> assert_equal [] (take_tr 2 []));
  "take zero elements" >:: (fun _ -> assert_equal [] (take_tr 0 [1;2]));
  "take more than list size" >:: (fun _ -> assert_equal [1;2] (take_tr 3 [1;2]));
  "take less than list size" >:: (fun _ -> assert_equal [1;2] (take_tr 2 [1;2;3]));
]
let _ = run_test_tt_main take_tr_tests

let is_unimodal_tests = "test for list unimodality" >::: [
  "empty list" >:: (fun _ -> assert_equal true (is_unimodal []));
  "const list" >:: (fun _ -> assert_equal true (is_unimodal [1;1;1]));
  "multiple element unimodal" >:: (fun _ -> assert_equal true (is_unimodal [1;2;4;1]));
  "multiple element nonunimodal" >:: (fun _ -> assert_equal false (is_unimodal [1;2;3;2;3]));
]
let _ = run_test_tt_main is_unimodal_tests
