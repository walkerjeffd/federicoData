test_that("water_year() works with one value", {
  expect_equal(water_year(lubridate::ymd(20190930), start_month = 10), 2019)
  expect_equal(water_year(lubridate::ymd(20191001), start_month = 10), 2020)
  expect_equal(water_year(lubridate::ymd(20190101), start_month = 10), 2019)
})

test_that("water_year() works with multiple values", {
  expect_equal(water_year(lubridate::ymd(c(20190930, 20191001)), start_month = 10), c(2019, 2020))
})

test_that("water_year() works with NAs", {
  expect_equal(water_year(lubridate::ymd(NA), start_month = 10), NA)
  expect_equal(water_year(lubridate::ymd(c(NA, 20191001)), start_month = 10), c(NA, 2020))
})
