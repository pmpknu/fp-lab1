import math

def count_divisors(n):
    sqrt_n = int(math.sqrt(n))
    count = sum(2 for i in range(1, sqrt_n + 1) if n % i == 0)
    if sqrt_n * sqrt_n == n:
        count -= 1
    return count

def first_triangle_number_with_divisors(limit):
    n = 1
    triangle = 1
    while True:
        divisors = count_divisors(triangle)
        if divisors > limit:
            return triangle
        n += 1
        triangle += n

limit = 500
result = first_triangle_number_with_divisors(limit)
print(result)