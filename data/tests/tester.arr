check:
  daily-max-for-month([list: 
      20150201, 300, 301,
      20150302, 42,
      20150323, 0.003, 123, 234,
      20150401, 0, 301, 302, 
      20150402, 500, 400, 
      20150423, 1, 323.230, 234,
      20150431, 1,
      20150501, 300, 
      20150502, 42, 5, 299, 
      20150623, 1, 123, 234], 4) is 
  [list: max-hz(1, 302), max-hz(2, 500), max-hz(23, 323.23), max-hz(31, 1)]

  daily-max-for-month([list: 
        20140630, 0, 0, 111,
        20140807, 0, 300, 22, 25,
        20140901, 101.1, 500, 1, 22,
        20140903, 123, 64, 300,
        20140920, 500, 1, 15, 11, 
        20141010, 400, 500, 
        20141101, 3,
        20141201, 34, 12.5, 0, 
        20141219, 23,
    20141220, 2.5, 2], 9) is [list: max-hz(1, 500), max-hz(3, 300), max-hz(20, 500)]

  #daily-max-for-month([list: 20151001, 200], 11) is empty
end