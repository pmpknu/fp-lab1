open Date

let count_sundays start_year end_year =
  let years = List.init (end_year - start_year + 1) (fun i -> start_year + i) in
  let aux year =
    let months = List.init 12 (fun m -> m + 1) in
    List.filter (fun month -> zellers_congruence year month 1 = 0) months |> List.length
  in
  List.fold_left (fun acc year -> acc + aux year) 0 years
;;

let%test _ = count_sundays 1901 2000 = 171
