let zellers_congruence year month day =
  let (year, month) =
    if month < 3 then (year - 1, month + 12) else (year, month)
  in
  let k = year mod 100 in
  let j = year / 100 in
  let f = day + (13 * (month + 1)) / 5 + k + k / 4 + j / 4 - (2 * j) in
  (f mod 7 + 7) mod 7 

let count_sundays start_year end_year =
  let years = List.init (end_year - start_year + 1) (fun i -> start_year + i) in
  let aux year =
    let months = List.init 12 (fun m -> m + 1) in
    List.filter (fun month -> zellers_congruence year month 1 = 0) months
    |> List.length
  in
  List.fold_left (fun acc year ->
    acc + aux year
  ) 0 years