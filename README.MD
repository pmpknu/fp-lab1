# Решение задач Project Euler на OCaml

## Лабораторная работа #1

- **Вариант:** _12_, _19_  
- **Преподаватель** Пенской Александр Владимирович  
- **Выполнил** `Горляков Даниил Петрович`, `367165`
- ИТМО, Санкт-Петербург, 2024  

## Описание работы

Данная лабораторная работа направлена на применение различных подходов функционального программирования для решения задач из Project Euler. Решенные задачи:
1. [**Задача 12**](./p12/README.md): Нахождение первого треугольного числа с более чем `N` делителями.
2. [**Задача 19**](./p19/README.md): Подсчет количества воскресений, выпадающих на первое число месяца за определенный период.

## Структура проекта

- [**p12/**](./p12/README.md) и [**p19/**](./p19/README.md) — директории с решениями задач 12 и 19 соответственно.
    - **lib/** — модули и файлы с реализациями различных подходов:
    - **test/** — тесты для проверки решений, написанные с использованием `OUnit2`.*

---
\* - только в [_p12_](./p12/README.md), в [_p19_](./p19/README.md) испльзуются `inline` тесты.


## [Решение задачи 12](./p12/README.md)
Ниже приведены тематические особенности реализаций (где использована рекурсия, хвостовая рекурсия, модульное решение и пр.)
### [Реализация рекурсией](./p12/lib/rec.ml)

```ocaml
let count_factors n =
  let rec aux i =
    if i * i > n
    then 0
    else if n mod i = 0
    then if i * i = n then 1 + aux (i + 1) else 2 + aux (i + 1)
    else aux (i + 1)
  in
  if n <= 0 then 0 else aux 1
;;
```

### [Реализация хвостовой рекурсией](./p12/lib/tailrec.ml)

```ocaml
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
```

### [Модульная реализация](./p12/lib/module.ml)

```ocaml
let result =
  List.filter (fun x -> count_factors_tail x > 500) triangular_numbers
  |> List.fold_left
       (fun acc x ->
         match acc with
         | Some _ -> acc
         | None -> Some x)
       None
;;

```

### [map](./p12/lib/map.ml)

```ocaml
let triangular_numbers limit =
  let generate_range n = List.init n (fun i -> i + 1) in
  List.map (fun n -> n * (n + 1) / 2) (generate_range limit)
;;
```

### [Ленивые коллекции](./p12/lib/lazy.ml)

```ocaml
let triangular_numbers =
  Seq.unfold
    (fun n ->
      let t = n * (n + 1) / 2 in
      Some (t, n + 1))
    1
;;
```
```ocaml
let find_first_triangular_with_factors n =
  Seq.find (fun x -> count_factors_tail x > n) triangular_numbers
;;
```

### [Итеративная реализация](./p12/lib/iterative.ml)
```ocaml
let count_divisors n =
  let count = ref 0 in
  let root = int_of_float (sqrt (float_of_int n)) in
  for i = 1 to root do
    if n mod i = 0 then if i = n / i then incr count else count := !count + 2
  done;
  !count
;;
```
```ocaml
let find_triangle_with_divisors limit =
  let found = ref false in
  let result = ref 0 in
  let n = ref 1 in
  while not !found do
    let tri_num = triangle_number !n in
    if count_divisors tri_num > limit
    then (
      result := tri_num;
      found := true);
    incr n
  done;
  !result
;;
```

### [Реализация на Python](./p12/solution.py)

```python
def count_divisors(n):
    sqrt_n = int(math.sqrt(n))
    count = sum(2 for i in range(1, sqrt_n + 1) if n % i == 0)
    if sqrt_n * sqrt_n == n:
        count -= 1
    return count
```
```python
def first_triangle_number_with_divisors(limit):
    n = 1
    triangle = 1
    while True:
        divisors = count_divisors(triangle)
        if divisors > limit:
            return triangle
        n += 1
        triangle += n
``` 

## [Решение задачи 19](./p19/README.md)
Ниже приведены тематические особенности реализаций (где использована рекурсия, хвостовая рекурсия, модульное решение и пр.)
### [Реализация рекурсией](./p19/lib/rec.ml)

```ocaml
let rec count_sundays year month day_of_week =
  if year > 2000
  then 0
  else (
    let new_count = if day_of_week = 0 then 1 else 0 in
    let days_this_month = days_in_month year month in
    let next_day_of_week = (day_of_week + days_this_month) mod 7 in
    if month == 12
    then new_count + count_sundays (year + 1) 1 next_day_of_week
    else new_count + count_sundays year (month + 1) next_day_of_week)
;;
```

### [Реализация хвостовой рекурсией](./p19/lib/tailrec.ml)

```ocaml
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
```

### [Модульная реализация](./p19/lib/module.ml)

```ocaml
let count_sundays start_year end_year =
  let years = List.init (end_year - start_year + 1) (fun i -> start_year + i) in
  let aux year =
    let months = List.init 12 (fun m -> m + 1) in
    List.filter (fun month -> zellers_congruence year month 1 = 0) months |> List.length
  in
  List.fold_left (fun acc year -> acc + aux year) 0 years
;;
```

### [map](./p19/lib/map.ml)

```ocaml
let count_sundays start_year end_year =
  let years = List.init (end_year - start_year + 1) (fun i -> start_year + i) in
  let aux year =
    let months = List.init 12 (fun m -> m + 1) in
    let map_filter month = if zellers_congruence year month 1 = 0 then 1 else 0 in
    List.fold_left ( + ) 0 (List.map map_filter months)
  in
  List.fold_left (fun acc year -> acc + aux year) 0 years
;;
```

### [Итеративная реализация](./p19/lib/iterative.ml)
```ocaml
let count_sundays start_year end_year =
  let count = ref 0 in
  for year = start_year to end_year do
    for month = 1 to 12 do
      if zellers_congruence year month 1 = 0 then incr count
    done
  done;
  !count
;;
```

### [Реализация на Python](./p19/solution.py)

```python
for year in range(1901, 2001):
    for month in range(1, 13):
        first_day = datetime(year, month, 1)
        if first_day.weekday() == 6:
            sundays_count += 1
```

## Выводы
OCaml показался мне достаточно простым и понятным по синтаксису, однако, при поиске сталкивался с примерами на Haskell и не понимал, почему первый так усложняет местами.

Я познакомился с такими функциями, как `List.map`, `List.filter` и `List.fold`, они сделали код более компактным и понятным. Использование функции `filter` для отсеивания неподходящих значений делает код более декларативным.

Также я воспользовался ленивыми коллекциями (`Seq`), для генерации бесконечного количества треугольных чисел, что за счет лени позволило обратиться только к нужному количеству треугольных чисел и не нужно было придумывать, как создавать следующее.

## Запуск тестов

Для запуска тестов с использованием OUnit2:

```bash
dune runtest
```

## Зависимости

Для сборки и запуска проекта необходимо иметь:
- OCaml >= 4.12.0
- Dune >= 2.0
- OUnit2
- ppx_inline_test 