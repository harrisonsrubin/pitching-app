test_that("joins on the UTC hour containing first pitch", {
  hourly <- tibble::tibble(
    venue_id = 19L,
    time_utc = as.POSIXct(c("2030-07-01 23:00", "2030-07-02 00:00"), tz = "UTC"),
    temp_c = c(30, 25), rh_pct = c(40, 50), pressure_hpa = c(1010, 1011))
  g <- tibble::tibble(game_pk = 1L, venue_id = 19L,
                      first_pitch_utc = as.POSIXct("2030-07-01 23:40", tz = "UTC"))
  out <- join_game_weather(g, hourly)
  expect_equal(nrow(out), 1)
  expect_equal(out$temp_c, 30)
  expect_equal(out$pressure_hpa, 1010)
})

test_that("a game with no matching hour keeps its row with NA weather", {
  hourly <- tibble::tibble(venue_id = 19L, time_utc = as.POSIXct("2030-07-01 10:00", tz = "UTC"),
                           temp_c = 20, rh_pct = 50, pressure_hpa = 1000)
  g <- tibble::tibble(game_pk = 2L, venue_id = 19L,
                      first_pitch_utc = as.POSIXct("2030-07-01 23:40", tz = "UTC"))
  out <- join_game_weather(g, hourly)
  expect_equal(nrow(out), 1)
  expect_true(is.na(out$temp_c))
})

test_that("parser reads GMT times as UTC and keeps nulls as NA", {
  w <- parse_open_meteo(jsonlite::read_json(test_path("fixtures/openmeteo_synthetic.json")))
  expect_equal(attr(w$time_utc, "tzone"), "UTC")
  expect_equal(format(w$time_utc[2], "%Y-%m-%d %H:%M"), "2030-07-01 23:00")
  expect_equal(w$temp_c, c(31, 30, 25))
  expect_true(is.na(w$pressure_hpa[3]))
})

test_that("open-meteo request asks for GMT and the three hourly variables", {
  r <- open_meteo_request(40.1, -75.2, as.Date("2030-04-01"), as.Date("2030-09-30"))
  expect_equal(r$query$timezone, "GMT")
  expect_equal(r$query$hourly, "temperature_2m,relative_humidity_2m,surface_pressure")
  expect_equal(r$query$start_date, "2030-04-01")
})
