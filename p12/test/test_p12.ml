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

(* Rec tests *)
let test_rec_count_factors _ =
  assert_equal 6 (Rec.count_factors 28);
  assert_equal 9 (Rec.count_factors 36);
  assert_equal 0 (Rec.count_factors 0)

let test_rec_find_triangular _ =
  assert_equal 28 (Rec.find_triangular_with_divisors 5 1);
  assert_equal 76576500 (Rec.find_triangular_with_divisors 500 1)
let suite =
  "Project Euler Problem 12 Tests" >::: [
    "Tailrec - count_factors" >:: test_tailrec_count_factors;
    "Tailrec - find_triangular" >:: test_tailrec_find_triangular;
    "Rec - count_factors" >:: test_rec_count_factors;
    "Rec - find_triangular" >:: test_rec_find_triangular;
]

let _ = run_test_tt_main suite
  