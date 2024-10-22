open OUnit2
open P12_lib

(* Tailrec tests *)
let test_tailrec_count_factors _ =
  OUnit2.assert_equal 6 (Tailrec.count_factors_tail 28);
  OUnit2.assert_equal 9 (Tailrec.count_factors_tail 36);
  OUnit2.assert_equal 0 (Tailrec.count_factors_tail 0)

let test_tailrec_find_triangular _ =
  OUnit2.assert_equal 28 (Tailrec.find_triangular_with_divisors_tail 5);
  OUnit2.assert_equal 76576500 (Tailrec.find_triangular_with_divisors_tail 500)

let suite =
  "Project Euler Problem 12 Tests" >::: [
    "Tailrec - count_factors" >:: test_tailrec_count_factors;
    "Tailrec - find_triangular" >:: test_tailrec_find_triangular;
  ]

let _ = run_test_tt_main suite
  