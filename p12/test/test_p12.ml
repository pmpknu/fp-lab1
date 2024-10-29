open OUnit2
open P12_lib

(* Tailrec tests *)
let test_tailrec_count_factors _ =
  assert_equal 6 (Tailrec.count_factors_tail 28);
  assert_equal 9 (Tailrec.count_factors_tail 36);
  assert_equal 0 (Tailrec.count_factors_tail 0)
;;

let test_tailrec_find_triangular _ =
  assert_equal 28 (Tailrec.find_triangular_with_divisors_tail 5);
  assert_equal 76576500 (Tailrec.find_triangular_with_divisors_tail 500)
;;

(* Rec tests *)
let test_rec_count_factors _ =
  assert_equal 6 (Rec.count_factors 28);
  assert_equal 9 (Rec.count_factors 36);
  assert_equal 0 (Rec.count_factors 0)
;;

let test_rec_find_triangular _ =
  assert_equal 28 (Rec.find_triangular_with_divisors 5 1);
  assert_equal 76576500 (Rec.find_triangular_with_divisors 500 1)
;;

(* Iterative tests *)
let test_iterative_count_divisors _ =
  assert_equal 6 (Iterative.count_divisors 28);
  assert_equal 9 (Iterative.count_divisors 36);
  assert_equal 0 (Iterative.count_divisors 0)
;;

let test_iterative_find_triangle _ =
  assert_equal 28 (Iterative.find_triangle_with_divisors 5);
  assert_equal 76576500 (Iterative.find_triangle_with_divisors 500)
;;

(* Module tests *)
let test_module_result _ =
  match Module.result with
  | Some x -> assert_equal 76576500 x
  | None -> assert_failure "Expected a result, got None"
;;

(* Map tests *)
let test_map_find_first _ =
  match Map.result with
  | Some x -> assert_equal 76576500 x
  | None -> assert_failure "Expected a result, got None"
;;

(* Lazy collections tests *)
let test_lazy_find_first _ =
  match Map.result with
  | Some x -> assert_equal 76576500 x
  | None -> assert_failure "Expected a result, got None"
;;

let suite =
  "Project Euler Problem 12 Tests"
  >::: [ "Tailrec - count_factors" >:: test_tailrec_count_factors
       ; "Tailrec - find_triangular" >:: test_tailrec_find_triangular
       ; "Rec - count_factors" >:: test_rec_count_factors
       ; "Rec - find_triangular" >:: test_rec_find_triangular
       ; "Iterative - count_factors" >:: test_iterative_count_divisors
       ; "Iterative - find_triangular" >:: test_iterative_find_triangle
       ; "Module - result" >:: test_module_result
       ; "Map - result" >:: test_map_find_first
       ; "Lazy - result" >:: test_lazy_find_first
       ]
;;

let _ = run_test_tt_main suite
