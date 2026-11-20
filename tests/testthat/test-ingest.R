test_that("the daily ingest re-checks a trailing window ending yesterday", {
  expect_equal(ingest_dates(as.Date("2030-06-10"), window = 5),
               as.Date(c("2030-06-05", "2030-06-06", "2030-06-07", "2030-06-08", "2030-06-09")))
})

test_that("weather is ingested with a lag because the archive runs days behind", {
  expect_equal(weather_date(as.Date("2030-06-10"), lag = 7), as.Date("2030-06-03"))
})

test_that("a weather frame that is mostly missing is not usable", {
  expect_false(weather_usable(tibble::tibble(temp_c = c(NA, NA, 20))))
  expect_true(weather_usable(tibble::tibble(temp_c = c(20, 21, 22, 23, 24, 25, 26, 27, 28, NA))))
  expect_false(weather_usable(tibble::tibble(temp_c = numeric())))
})
