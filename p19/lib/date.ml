let is_leap year = (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0

let days_in_month year month =
  match month with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if is_leap year then 29 else 28
  | _ -> failwith "Invalid month"
;;

let zellers_congruence year month day =
  let year, month = if month < 3 then year - 1, month + 12 else year, month in
  let k = year mod 100 in
  let j = year / 100 in
  let f = day + (13 * (month + 1) / 5) + k + (k / 4) + (j / 4) - (2 * j) in
  ((f mod 7) + 7) mod 7
;;
