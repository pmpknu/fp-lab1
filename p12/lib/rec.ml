let triangle_number n =
  n * (n + 1) / 2

let count_factors n =
  let rec aux i =
    if i * i > n then 0
    else if n mod i = 0 then
      if i * i = n then 1 + aux (i + 1)  
      else 2 + aux (i + 1)               
    else aux (i + 1)
  in
  if n <= 0 then 0 else aux 1

let rec find_triangular_with_divisors target n =
  let tri_num = triangle_number n in
  let factors_count = count_factors tri_num in
  if factors_count > target then tri_num
  else find_triangular_with_divisors target (n + 1)

let result = find_triangular_with_divisors 500 1