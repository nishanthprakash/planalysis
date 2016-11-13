provide *
###############################################################################

data Report:
  | max-hz(day :: Number, max-reading :: Number)
end

test-nums-1 = [list: 20140901, 101.1, 100, 120, 20140902, 500, 450, 100, 
  20140904, 90, 300, 299.9, 20140905, 400, 20141001, 20, 20141002, 400, 
  500, 300, 20141101, 30]

test-nums-2 = [list: 20150101, 100.1, 97, 20150102, 10, 75, 200.4, 
  20150115, 309, 407, 20150301, 10 ]

test-nums-3 = [list: 20150401, 300]

fun daily-max-for-month(sensor-data :: List<Number>, month :: Number) -> List<Report>:
  doc: ``` Takes a list of numbers, parses the months, and returns 
       a list of Reports with the highest reading per day.```
  fun parse(l :: List<Number>, cur-month :: Number, 
      cur-report :: List<Report>) -> List<Report>:
    doc:```Takes a list of numbers, a month, and a list of reports, and 
        adds Reports if the month matches, and swaps the max-reading if 
        the reading is higher than the previous value.```
    cases(List) l:
      |empty => cur-report
      |link(f,r) => 
        if f > 500:
          check-month = string-to-number(string-substring(
              num-to-string(f), 4,  6)).value
          if check-month == month:
            parsed-day = string-to-number(string-substring(
                num-to-string(f), 6,  8)).value
            parse(r, check-month, link(max-hz(parsed-day, 0), cur-report))
          else:
            parse(r, check-month, cur-report)
          end
        else if (f <= 500) and (cur-month == month):
          if f > cur-report.first.max-reading:
            parse(r, cur-month, link(max-hz(cur-report.first.day, f), 
                cur-report.rest))
          else:
            parse(r, cur-month, cur-report)
          end
        else:
          parse(r, cur-month, cur-report)
        end
    end
  end
  parse(sensor-data, month, empty).reverse()         
where:
  daily-max-for-month(test-nums-1, 4) is empty
  daily-max-for-month(test-nums-1, 10) is 
  [list: max-hz(1, 20), max-hz(2, 500)]
  daily-max-for-month(test-nums-1, 9) is 
  [list: max-hz(1, 120), max-hz(2, 500), max-hz(4, 300), max-hz(5, 400)]
  daily-max-for-month(test-nums-1, 11) is [list: max-hz(1, 30)]
  daily-max-for-month(test-nums-2, 3) is [list: max-hz(1, 10)]
  daily-max-for-month(test-nums-2, 1) is 
  [list: max-hz(1, 100.1), max-hz(2, 200.4), max-hz(15, 407)]
  daily-max-for-month(test-nums-2, 2) is empty
  daily-max-for-month(test-nums-3, 4) is [list: max-hz(1, 300)] 
end