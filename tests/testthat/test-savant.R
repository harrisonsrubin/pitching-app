fx_text <- function() paste(readLines(test_path("fixtures/savant_synthetic.csv")), collapse = "\n")

test_that("parses, filters to regular season, builds pitch_uid and season", {
  d <- parse_statcast_csv(fx_text(), "MLB")
  expect_equal(nrow(d), 5)
  expect_true(all(d$level == "MLB"))
  expect_true(all(d$game_type == "R"))
  expect_match(d$pitch_uid, "^\\d+-\\d+-\\d+$")
  expect_equal(d$pitch_uid[1], "900001-1-1")
  expect_equal(unique(d$season), 2030L)
  expect_s3_class(d$game_date, "Date")
  expect_type(d$release_speed, "double")
  expect_true(is.na(d$release_speed[d$pitch_uid == "900001-2-2"]))
  expect_false(anyDuplicated(d$pitch_uid) > 0)
})

test_that("a byte-order mark on the first column name is removed", {
  txt <- paste0("﻿", fx_text())
  expect_true("pitch_type" %in% names(parse_statcast_csv(txt, "MLB")))
})

test_that("header-only returns zero rows with the same columns", {
  hdr <- readLines(test_path("fixtures/savant_synthetic.csv"))[1]
  d <- parse_statcast_csv(hdr, "AAA")
  expect_equal(nrow(d), 0)
  expect_true(all(c("pitch_uid", "level", "season") %in% names(d)))
})

test_that("an HTML body is a bad response", {
  expect_error(parse_statcast_csv("<html>busy</html>", "MLB"), class = "pv_bad_response")
  expect_error(parse_statcast_csv("  \n<!DOCTYPE html>", "MLB"), class = "pv_bad_response")
})

test_that("AAA request adds the minors flags; MLB does not", {
  m <- savant_request(as.Date("2025-06-01"), "MLB")
  a <- savant_request(as.Date("2025-06-01"), "AAA")
  expect_false(identical(m, a))
  expect_equal(a$query$minors, "true")
  expect_equal(a$query$hfLevel, "AAA|")
  expect_null(m$query$minors)
  expect_equal(m$query$game_date_gt, "2025-06-01")
  expect_equal(m$query$game_date_lt, "2025-06-01")
})

test_that("unknown level is rejected", {
  expect_error(savant_request(as.Date("2025-06-01"), "A"))
})
