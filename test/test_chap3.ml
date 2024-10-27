open OUnit2
open Chap3

let make_product_test name product_version expected_output input =
  name >::(fun _ -> assert_equal expected_output (product_version input) ~printer:string_of_int)

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