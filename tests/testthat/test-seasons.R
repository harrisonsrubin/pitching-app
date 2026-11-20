schedule_fixture <- function(dates_by_season) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  for (s in names(dates_by_season)) {
    p <- file.path(dir, "raw", "statsapi", "schedule", "MLB")
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
    arrow::write_parquet(tibble::tibble(game_pk = seq_along(dates_by_season[[s]]),
                                        game_date = as.Date(dates_by_season[[s]])),
                         file.path(p, paste0(s, ".parquet")))
  }
  dir
}

test_that("last_season is 2026 when only 2026 schedules exist", {
  dir <- schedule_fixture(list(`2025` = "2025-04-01", `2026` = "2026-03-26"))
  withr::local_envvar(PV_DATA_DIR = dir)
  expect_identical(last_season(as.Date("2026-11-20")), 2026L)
})

test_that("last_season rolls to 2027 once a 2027 game date has passed", {
  dir <- schedule_fixture(list(`2026` = "2026-03-26", `2027` = c("2027-03-25", "2027-09-30")))
  withr::local_envvar(PV_DATA_DIR = dir)
  expect_identical(last_season(as.Date("2027-03-24")), 2026L)
  expect_identical(last_season(as.Date("2027-03-26")), 2027L)
})

test_that("last_season never drops below 2026", {
  dir <- schedule_fixture(list(`2025` = "2025-04-01"))
  withr::local_envvar(PV_DATA_DIR = dir)
  expect_identical(last_season(as.Date("2025-06-01")), 2026L)
})

test_that("cfg seasons follow last_season", {
  dir <- schedule_fixture(list(`2027` = "2027-03-25"))
  withr::local_envvar(PV_DATA_DIR = dir)
  withr::local_options(pv.today = as.Date("2027-04-01"))
  c0 <- cfg()
  expect_equal(c0$seasons_mlb, 2020:2027)
  expect_equal(c0$seasons_aaa, 2023:2027)
  expect_equal(c0$seasons_stats, 2010:2027)
  expect_equal(c0$seasons_train, 2023:2027)
  expect_equal(c0$seasons_ref, 2023:2025)
})

test_that("last_season rolls over from ingested Statcast days alone (no cached schedule)", {
  dir <- withr::local_tempdir()
  p <- file.path(dir, "raw", "statcast", "MLB", "2027")
  dir.create(p, recursive = TRUE)
  arrow::write_parquet(tibble::tibble(a = 1), file.path(p, "2027-03-26.parquet"))
  withr::local_envvar(PV_DATA_DIR = dir)
  expect_identical(last_season(as.Date("2027-03-27")), 2027L)
  expect_identical(last_season(as.Date("2027-03-25")), 2026L)
})
