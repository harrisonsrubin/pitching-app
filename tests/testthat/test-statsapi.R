read_fx <- function(name) jsonlite::read_json(test_path("fixtures", name))

test_that("schedule parser keeps finished games with UTC first pitch", {
  s <- parse_schedule(read_fx("schedule_synthetic.json"), "MLB")
  expect_equal(sort(s$game_pk), c(900001L, 900003L))
  expect_equal(attr(s$first_pitch_utc, "tzone"), "UTC")
  expect_equal(format(s$first_pitch_utc[s$game_pk == 900001], "%H:%M"), "23:05")
  expect_true(all(c("game_date", "venue_id", "level", "double_header",
                    "weather_condition", "weather_temp_f", "weather_wind") %in% names(s)))
  expect_equal(s$weather_temp_f[s$game_pk == 900001], 81)
  expect_true(is.na(s$weather_condition[s$game_pk == 900003]))
  expect_true(all(s$level == "MLB"))
})

test_that("game_days returns sorted unique dates of finished games", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  local_global_binding(fetch_schedule = function(season, level) {
    tibble::tibble(game_date = as.Date(c("2030-06-02", "2030-06-01", "2030-06-02")))
  })
  expect_equal(game_days(2030, "MLB"), as.Date(c("2030-06-01", "2030-06-02")))
})

test_that("venue parser returns coordinates, elevation, roof; missing location is NA", {
  v <- parse_venues(read_fx("venues_synthetic.json"))
  expect_equal(nrow(v), 3)
  expect_true(all(c("venue_id", "venue_name", "lat", "lon", "elevation_ft", "roof_type", "azimuth_deg") %in% names(v)))
  expect_equal(v$elevation_ft[v$venue_id == 1002], 1086)
  expect_true("Retractable" %in% v$roof_type)
  expect_true(is.na(v$lat[v$venue_id == 1003]))
})

test_that("build_parks tags levels and seasons and applies overrides last", {
  sched <- tibble::tibble(
    venue_id = c(1001L, 1001L, 1002L, 1003L),
    level = c("MLB", "AAA", "AAA", "AAA"),
    game_date = as.Date(c("2024-05-01", "2025-05-01", "2023-05-01", "2026-05-01")))
  venues <- parse_venues(read_fx("venues_synthetic.json"))
  overrides <- tibble::tibble(venue_id = 1003L, lat = 35.0, lon = -106.0, elevation_ft = 5100)
  p <- build_parks(sched, venues, overrides)
  expect_equal(p$level[p$venue_id == 1001], "both")
  expect_equal(p$first_season[p$venue_id == 1001], 2024L)
  expect_equal(p$last_season[p$venue_id == 1001], 2025L)
  expect_equal(p$elevation_ft[p$venue_id == 1003], 5100)
  expect_equal(p$lat[p$venue_id == 1003], 35.0)
})
