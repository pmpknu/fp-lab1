let triangle_number n = n * (n + 1) / 2

let count_divisors n =
  let count = ref 0 in
  let root = int_of_float (sqrt (float_of_int n)) in
  for i = 1 to root do
    if n mod i = 0 then if i = n / i then incr count else count := !count + 2
  done;
  !count

let find_triangle_with_divisors limit =
  let found = ref false in
  let result = ref 0 in
  let n = ref 1 in
  while not !found do
    let tri_num = triangle_number !n in
    if count_divisors tri_num > limit then (
      result := tri_num;
      found := true);
    incr n
  done;
  !result

let result = find_triangle_with_divisors 500
