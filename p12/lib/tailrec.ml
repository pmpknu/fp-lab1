let count_factors n =
  let rec aux i count =
    if i * i > n then count
    else if n mod i = 0 then aux (i + 1) (count + if i * i = n then 1 else 2)
    else aux (i + 1) count
  in
  if n <= 0 then 0 else aux 1 0

let find_triangular_with_divisors target =
  let rec aux n tri_num =
    let factors_count = count_factors tri_num in
    if factors_count > target then tri_num
    else aux (n + 1) (tri_num + n + 1)
  in
  aux 1 1

let result = find_triangular_with_divisors 500
