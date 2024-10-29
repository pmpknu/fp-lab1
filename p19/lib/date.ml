let is_leap year = (year mod 4 = 0 && year mod 100 <> 0) || year mod 400 = 0

let%test "is_leap for leap year 2000" = is_leap 2000 = true
let%test "is_leap for leap year 2020" = is_leap 2020 = true
let%test "is_leap for non-leap year 1900" = is_leap 1900 = false
let%test "is_leap for non-leap year 2023" = is_leap 2023 = false

let days_in_month year month =
  match month with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if is_leap year then 29 else 28
  | _ -> failwith "Invalid month"
;;

let%test "days_in_month for February in leap year" = days_in_month 2020 2 = 29
let%test "days_in_month for February in non-leap year" = days_in_month 2021 2 = 28
let%test "days_in_month for January" = days_in_month 2021 1 = 31
let%test "days_in_month for April" = days_in_month 2021 4 = 30

let zellers_congruence year month day =
  let year, month = if month < 3 then year - 1, month + 12 else year, month in
  let k = year mod 100 in
  let j = year / 100 in
  let f = day + (13 * (month + 1) / 5) + k + (k / 4) + (j / 4) - (2 * j) in
  ((f mod 7) + 7) mod 7
;;

let%test "zellers_congruence for 1 Jan 1900 (Monday)" = zellers_congruence 1900 1 1 = 2
let%test "zellers_congruence for 1 Jan 2000 (Saturday)" = zellers_congruence 2000 1 1 = 0
let%test "zellers_congruence for 1 Mar 2021 (Monday)" = zellers_congruence 2021 2 28 = 1
let%test "zellers_congruence for 31 Dec 1999 (Friday)" = zellers_congruence 1999 12 31 = 6
