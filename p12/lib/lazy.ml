let triangular_numbers =
  Seq.unfold
    (fun n ->
      let t = n * (n + 1) / 2 in
      Some (t, n + 1))
    1
;;

let count_factors_tail n =
  let rec aux i count =
    if i * i > n
    then count
    else if n mod i = 0
    then aux (i + 1) (count + if i * i = n then 1 else 2)
    else aux (i + 1) count
  in
  if n <= 0 then 0 else aux 1 0
;;

let find_first_triangular_with_factors n =
  Seq.find (fun x -> count_factors_tail x > n) triangular_numbers
;;

let result = find_first_triangular_with_factors 500
