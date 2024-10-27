from datetime import datetime, timedelta

sundays_count = 0

for year in range(1901, 2001):
    for month in range(1, 13):
        first_day = datetime(year, month, 1)
        if first_day.weekday() == 6:
            sundays_count += 1

print(sundays_count)
