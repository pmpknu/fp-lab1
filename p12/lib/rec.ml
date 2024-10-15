let triangle_number n =
  n * (n + 1) / 2

let count_factors n =
  let rec aux i count =
    if i * i > n then count
    else if n mod i = 0 then aux (i + 1) (count + if i * i = n then 1 else 2)
    else aux (i + 1) count
  in
  if n <= 0 then 0 else aux 1 0