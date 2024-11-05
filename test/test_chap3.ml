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

let powerset_tests = "tests for powerset creation" >::: [
  "empty list" >:: (fun _ -> assert_equal [[]] (powerset []));
  "one element" >:: (fun _ -> assert_equal [[1];[]] (powerset [1]));
  "multiple elements" >:: (fun _ -> assert_equal [[1;2];[1];[2];[]] (powerset [1;2]))
]
let _ = run_test_tt_main powerset_tests

let () = print_int_list [1;2;3]
let () = print_int_list' [1;2;3;4]

let safe_hd_tests = "tests for safe hd" >::: [
  "empty list" >:: (fun _ -> assert_equal None (safe_hd []));
  "one element" >:: (fun _ -> assert_equal (Some 1) (safe_hd [1]));
  "multiple elements" >:: (fun _ -> assert_equal (Some 1) (safe_hd [1;2]))
]
let safe_tl_tests = "tests for safe tl" >::: [
  "empty list" >:: (fun _ -> assert_equal None (safe_tl []));
  "one element" >:: (fun _ -> assert_equal (Some []) (safe_tl [1]));
  "multiple elements" >:: (fun _ -> assert_equal (Some [2;3]) (safe_tl [1;2;3]))
]
let _ = run_test_tt_main safe_hd_tests
let _ = run_test_tt_main safe_tl_tests

let poke_with_45hp = {name="poke1"; hp=45; ptype=Fire}
let poke_with_55hp = {name="poke2"; hp=55; ptype=Normal}
let poke_with_65hp = {name="poke3"; hp=65; ptype=Fire}
let pokefun_tests = "tests for pokemon with max hp" >::: [
  "empty list" >:: (fun _ -> assert_equal None (max_hp []));
  "one element" >:: (fun _ -> assert_equal (Some poke_with_45hp) (max_hp [poke_with_45hp]));
  "multiple elements" >:: (fun _ -> assert_equal (Some poke_with_65hp) (max_hp [poke_with_45hp;poke_with_65hp;poke_with_55hp]));
]

let _ = run_test_tt_main pokefun_tests

let date_before_tests = "tests for check if date is before other one" >::: [
  "same dates" >:: (fun _ -> assert_equal false (is_before (1995, 12, 12) (1995, 12, 12)));
  "same year" >:: (fun _ -> assert_equal true (is_before (1995, 11, 12) (1995, 12, 12)));
  "same month" >:: (fun _ -> assert_equal true (is_before (1995, 11, 12) (1995, 11, 13)));
]
let _ = run_test_tt_main date_before_tests

let earliest_date_tests = "tests for earliest date from list" >::: [
  "empty list" >:: (fun _ -> assert_equal None (earliest []));
  "multiple dates" >:: (fun _ -> assert_equal (Some (1995, 11, 12)) (earliest [(1997, 12, 12);(1995, 11, 12);(1996, 12, 12)]));
]
let _ = run_test_tt_main earliest_date_tests

let assoc_list_tests = "tests for association list" >::: [
  "list is constructed properly" >:: (fun _ -> assert_equal [(1,"one");(2,"two");(3,"three")] assoc_list);
  "existing el lookup" >:: (fun _ -> assert_equal (Some "two") key_two);
  "non existent el lookup" >:: (fun _ -> assert_equal None key_four);
]
let _ = run_test_tt_main assoc_list_tests

let quadrant_tests = "tests for coordinates quadrants" >::: [
  "origin" >:: (fun _ -> assert_equal None (quadrant (0,0)));
  "first quadrant" >:: (fun _ -> assert_equal (Some I) (quadrant (1,1)));
  "second quadrant" >:: (fun _ -> assert_equal (Some II) (quadrant (-1,1)));
  "third quadrant" >:: (fun _ -> assert_equal (Some III) (quadrant (-1,-1)));
  "fourth quadrant" >:: (fun _ -> assert_equal (Some IV) (quadrant (1,-1)));
  "on x axis" >:: (fun _ -> assert_equal None (quadrant (0,1)));
  "on y axis" >:: (fun _ -> assert_equal None (quadrant (1,0)))
]
let _ = run_test_tt_main quadrant_tests

let quadrant_when_tests = "tests for coordinates quadrants" >::: [
  "origin" >:: (fun _ -> assert_equal None (quadrant_when (0,0)));
  "first quadrant" >:: (fun _ -> assert_equal (Some I) (quadrant_when (1,1)));
  "second quadrant" >:: (fun _ -> assert_equal (Some II) (quadrant_when (-1,1)));
  "third quadrant" >:: (fun _ -> assert_equal (Some III) (quadrant_when (-1,-1)));
  "fourth quadrant" >:: (fun _ -> assert_equal (Some IV) (quadrant_when (1,-1)));
  "on x axis" >:: (fun _ -> assert_equal None (quadrant_when (0,1)));
  "on y axis" >:: (fun _ -> assert_equal None (quadrant_when (1,0)))
]
let _ = run_test_tt_main quadrant_when_tests

let tree_depth_test = "test for tree depth" >::: [
  "empty tree" >:: (fun _ -> assert_equal 0 (depth Leaf));
  "only root" >:: (fun _ -> assert_equal 1 (depth (Node(1,Leaf, Leaf))));
  "left subtree deeper" >:: (fun _ -> assert_equal 2 (depth (Node(1,Node(2,Leaf,Leaf), Leaf))));
  "right subtree deeper" >:: (fun _ -> assert_equal 2 (depth (Node(1,Leaf,Node(2,Leaf,Leaf)))));
]
let _ = run_test_tt_main 
