open Date

let count_sundays start_year end_year =
  let count = ref 0 in
  for year = start_year to end_year do
    for month = 1 to 12 do
      if zellers_congruence year month 1 = 0 then incr count
    done
  done;
 !count
;;
