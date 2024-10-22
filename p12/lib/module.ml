let triangle_number n = n * (n + 1) / 2

let count_factors_tail n =
  let rec aux i count =
    if i * i > n then count
    else if n mod i = 0 then aux (i + 1) (count + if i * i = n then 1 else 2)
    else aux (i + 1) count
  in
  if n <= 0 then 0 else aux 1 0

let triangular_numbers = List.init 15000 (fun n -> triangle_number (n + 1))

let result =
  List.filter (fun x -> count_factors_tail x > 500) triangular_numbers
  |> List.fold_left
       (fun acc x -> match acc with Some _ -> acc | None -> Some x)
       None
