let triangle_number n =
  n * (n + 1) / 2

let triangular_numbers limit =
  let generate_range n =
    List.init n (fun i -> i + 1)
  in
  List.map triangle_number (generate_range limit)

let count_factors_tail n =
  let rec aux i count =
    if i * i > n then count
    else if n mod i = 0 then aux (i + 1) (count + if i * i = n then 1 else 2)
    else aux (i + 1) count
  in
  if n <= 0 then 0 else aux 1 0

let find_first_triangular_with_factors limit factor_count_threshold =
  let triangulars = triangular_numbers limit in
  List.find_opt (fun x -> count_factors_tail x > factor_count_threshold) triangulars

let result = find_first_triangular_with_factors 15000 500
