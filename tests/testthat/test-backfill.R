fake_days <- function(season, level) as.Date(c("2025-04-01", "2025-04-02", "2025-04-03"))

fake_fetch <- function(date, level) {
  if (date == as.Date("2025-04-03")) return(tibble::tibble(pitch_uid = character(), game_date = as.Date(character())))
  tibble::tibble(pitch_uid = paste0(as.integer(date), "-1-1"), game_date = date)
}

test_that("skips existing days, writes new days, logs empties without writing", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  store_write(tibble::tibble(pitch_uid = "x", game_date = as.Date("2025-04-01")),
              statcast_day_key("MLB", as.Date("2025-04-01")))
  log <- backfill_statcast("MLB", 2025, fetch = fake_fetch, days = fake_days, expected = NULL, retry_wait = 0)
  expect_equal(log$status, c("skipped", "written", "empty_error"))
  expect_equal(log$rows, c(NA_integer_, 1L, 0L))
  expect_true(store_exists(statcast_day_key("MLB", as.Date("2025-04-02"))))
  expect_false(store_exists(statcast_day_key("MLB", as.Date("2025-04-03"))))
})

test_that("an empty day is retried once before it is logged as an error", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  calls <- 0L
  flaky <- function(date, level) {
    calls <<- calls + 1L
    if (calls == 1L) return(tibble::tibble(pitch_uid = character(), game_date = as.Date(character())))
    tibble::tibble(pitch_uid = "1-1-1", game_date = date)
  }
  one_day <- function(season, level) as.Date("2025-04-05")
  log <- backfill_statcast("MLB", 2025, fetch = flaky, days = one_day, expected = NULL, retry_wait = 0)
  expect_equal(calls, 2L)
  expect_equal(log$status, "written")
})

test_that("a fetch error on one day is logged and the run continues", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  boom <- function(date, level) {
    if (date == as.Date("2025-04-01")) stop("network down")
    tibble::tibble(pitch_uid = "1-1-1", game_date = date)
  }
  log <- backfill_statcast("MLB", 2025, fetch = boom, days = fake_days, expected = NULL, retry_wait = 0)
  expect_equal(log$status, c("fetch_error", "written", "written"))
  expect_false(store_exists(statcast_day_key("MLB", as.Date("2025-04-01"))))
})

test_that("day keys follow raw/statcast/<level>/<season>/<date>.parquet", {
  expect_equal(statcast_day_key("AAA", as.Date("2024-07-04")),
               "raw/statcast/AAA/2024/2024-07-04.parquet")
})

test_that("compaction dedupes resumed-game pitches, keeping the later date", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  store_write(tibble::tibble(pitch_uid = "1-1-1", game_date = as.Date("2025-05-01")),
              statcast_day_key("MLB", as.Date("2025-05-01")))
  store_write(tibble::tibble(pitch_uid = c("1-1-1", "2-1-1"), game_date = as.Date("2025-05-02")),
              statcast_day_key("MLB", as.Date("2025-05-02")))
  key <- compact_season("MLB", 2025)
  expect_equal(key, "raw/statcast/MLB/2025.parquet")
  d <- store_read(key)
  expect_equal(nrow(d), 2)
  expect_equal(d$game_date[d$pitch_uid == "1-1-1"], as.Date("2025-05-02"))
})

# Retries, partial days, and compaction
two_games <- function(date, level) c(1L, 2L)
fetch_game1_only <- function(date, level) tibble::tibble(pitch_uid = "1-1-1", game_pk = 1L, game_date = date)
one_day_at <- function(d) function(season, level) as.Date(d)

test_that("a recent day missing a scheduled game is retried, then not written", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  calls <- 0L
  f <- function(date, level) { calls <<- calls + 1L; fetch_game1_only(date, level) }
  log <- backfill_statcast("MLB", 2030, fetch = f, days = one_day_at("2030-06-10"),
                           expected = two_games, today = as.Date("2030-06-12"), retry_wait = 0)
  expect_equal(calls, 2L)
  expect_equal(log$status, "incomplete_error")
  expect_false(store_exists(statcast_day_key("MLB", as.Date("2030-06-10"))))
})

test_that("an old day missing a game is written but flagged partial", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  log <- backfill_statcast("MLB", 2030, fetch = fetch_game1_only, days = one_day_at("2030-04-01"),
                           expected = two_games, today = as.Date("2030-06-12"), retry_wait = 0)
  expect_equal(log$status, "written_partial")
  expect_true(store_exists(statcast_day_key("MLB", as.Date("2030-04-01"))))
})

test_that("a fetch error (e.g. an HTML busy page) is retried before giving up", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  calls <- 0L
  f <- function(date, level) {
    calls <<- calls + 1L
    if (calls == 1L) parse_statcast_csv("<html>busy</html>", level)
    tibble::tibble(pitch_uid = "1-1-1", game_pk = 1L, game_date = date)
  }
  log <- backfill_statcast("MLB", 2030, fetch = f, days = one_day_at("2030-04-01"),
                           expected = function(date, level) 1L, retry_wait = 0)
  expect_equal(calls, 2L)
  expect_equal(log$status, "written")
})

test_that("backfill_failed is TRUE only when some day ended in an error status", {
  expect_true(backfill_failed(tibble::tibble(status = c("written", "incomplete_error"))))
  expect_true(backfill_failed(tibble::tibble(status = "fetch_error")))
  expect_false(backfill_failed(tibble::tibble(status = c("written", "skipped", "written_partial"))))
})

test_that("compaction streams through duckdb and handles day files with different columns", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  store_write(tibble::tibble(pitch_uid = "1-1-1", game_date = as.Date("2025-05-01"), x = 1),
              statcast_day_key("MLB", as.Date("2025-05-01")))
  store_write(tibble::tibble(pitch_uid = c("1-1-1", "2-1-1"), game_date = as.Date("2025-05-02"),
                             x = c(2, 3), y = c("a", "b")),
              statcast_day_key("MLB", as.Date("2025-05-02")))
  d <- store_read(compact_season("MLB", 2025))
  expect_equal(nrow(d), 2)
  expect_equal(d$x[d$pitch_uid == "1-1-1"], 2)
  expect_true("y" %in% names(d))
})
