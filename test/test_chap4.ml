open OUnit2
open Solutions.Chap4

let repeat_test =
  "tests for repeat function"
  >::: [
         ("no repeat" >:: fun _ -> assert_equal 1 (repeat succ 1 0));
         ("repeat multiple times" >:: fun _ -> assert_equal 4 (repeat succ 1 3));
       ]

let _ = run_test_tt_main repeat_test

let fold_product_test =
  "tests for product out of list of floats"
  >::: [
         ("empty list prod left" >:: fun _ -> assert_equal 1. (product_left []));
         ( "empty list prod right" >:: fun _ ->
           assert_equal 1. (product_right []) );
         ( "list prod right" >:: fun _ ->
           assert_equal 2. (product_left [ 1.; 2. ]) );
         ( "list prod right" >:: fun _ ->
           assert_equal 2. (product_right [ 1.; 2. ]) );
       ]

let _ = run_test_tt_main fold_product_test

let sum_of_odd_cubes_test =
  "tests for sum of cubed odd numbers in a list"
  >::: [
         ("empty list" >:: fun _ -> assert_equal 0 (sum_cube_odd 0));
         ("empty list prod right" >:: fun _ -> assert_equal 153 (sum_cube_odd 5));
       ]

let _ = run_test_tt_main sum_of_odd_cubes_test
let pred x = x = 2

let exists_test =
  "tests for existence of an element in a list"
  >::: [
         ("empty list rec" >:: fun _ -> assert_equal false (exists_rec pred []));
         ( "element missing rec" >:: fun _ ->
           assert_equal false (exists_rec pred [ 1; 3 ]) );
         ( "element present rec" >:: fun _ ->
           assert_equal true (exists_rec pred [ 1; 2 ]) );
         ( "empty list fold" >:: fun _ ->
           assert_equal false (exists_fold pred []) );
         ( "element missing fold" >:: fun _ ->
           assert_equal false (exists_fold pred [ 1; 3 ]) );
         ( "element present fold" >:: fun _ ->
           assert_equal true (exists_fold pred [ 1; 2 ]) );
       ]

let _ = run_test_tt_main exists_test

let account_balance_test =
  "tests for calculation of account balance"
  >::: [
         ("no debit rec" >:: fun _ -> assert_equal 100. (balance_rec 100. []));
         ( "debit list rec" >:: fun _ ->
           assert_equal 10. (balance_rec 100. [ 50.; 40. ]) );
         ("no debit left" >:: fun _ -> assert_equal 100. (balance_left 100. []));
         ( "debit list left" >:: fun _ ->
           assert_equal 10. (balance_left 100. [ 50.; 40. ]) );
         ( "no debit right" >:: fun _ ->
           assert_equal 100. (balance_right 100. []) );
         ( "debit list right" >:: fun _ ->
           assert_equal 10. (balance_right 100. [ 50.; 40. ]) );
       ]

let _ = run_test_tt_main account_balance_test

let more_list_fun_test =
  "tests miscelleaneous list functions"
  >::: [
         ( "greater than three" >:: fun _ ->
           assert_equal [ 4; 5 ] (greater_than_three [ 3; 4; 1; 5; 2 ]) );
         ( "add one" >:: fun _ ->
           assert_equal [ 51.; 41. ] (add_one [ 50.; 40. ]) );
         ( "concat with separator" >:: fun _ ->
           assert_equal "hi,bye" (concat_with_separator "," [ "hi"; "bye" ]) );
       ]

let _ = run_test_tt_main more_list_fun_test
let list_no_dups = [ (1, "one"); (2, "two"); (3, "three") ]
let list_with_dups = [ (1, "one"); (2, "two"); (3, "three"); (2, "two") ]
let key_list = [ 1; 2; 3 ]

let association_list_dup_test =
  "tests for duplicate removal from association list"
  >::: [
         ("no duplicates" >:: fun _ -> assert_equal key_list (keys list_no_dups));
         ( "with duplicates" >:: fun _ ->
           assert_equal key_list (keys list_with_dups) );
       ]

let _ = run_test_tt_main association_list_dup_test

let matrix_validator_test =
  "tests for matrix validator"
  >::: [
         ("empty list" >:: fun _ -> assert_equal false (is_valid_matrix []));
         ( "valid matrix" >:: fun _ ->
           assert_equal true (is_valid_matrix [ [ 1; 2 ]; [ 4; 3 ] ]) );
         ( "invalid matrix" >:: fun _ ->
           assert_equal false (is_valid_matrix [ [ 1; 2 ]; [ 4 ] ]) );
       ]

let _ = run_test_tt_main matrix_validator_test

let row_vectors_addition_test =
  "tests for row vector addition"
  >::: [
         ("empty vectors" >:: fun _ -> assert_equal [] (add_row_vectors [] []));
         ( "different lengths" >:: fun _ ->
           assert_raises (Invalid_argument "List.map2") (fun _ ->
               add_row_vectors [ 1; 2 ] [ 4 ]) );
         ( "two vectors" >:: fun _ ->
           assert_equal [ 5; 7 ] (add_row_vectors [ 1; 2 ] [ 4; 5 ]) );
       ]

let _ = run_test_tt_main row_vectors_addition_test

let matrix_addition_test =
  "tests for row vector addition"
  >::: [
         ( "two vectors" >:: fun _ ->
           assert_equal
             [ [ 2; 4 ]; [ 8; 6 ] ]
             (add_matrices [ [ 1; 2 ]; [ 4; 3 ] ] [ [ 1; 2 ]; [ 4; 3 ] ]) );
       ]

let _ = run_test_tt_main matrix_addition_test
