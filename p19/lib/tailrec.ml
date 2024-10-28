open Date

let rec count_sundays year month day_of_week count =
  if year > 2000
  then count
  else (
    let new_count = if day_of_week = 0 then count + 1 else count in
    let days_this_month = days_in_month year month in
    let next_day_of_week = (day_of_week + days_this_month) mod 7 in
    if month == 12
    then count_sundays (year + 1) 1 next_day_of_week new_count
    else count_sundays year (month + 1) next_day_of_week new_count)
;;
