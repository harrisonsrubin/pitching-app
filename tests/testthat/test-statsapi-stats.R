read_fx <- function(name) jsonlite::read_json(test_path("fixtures", name))

test_that("innings strings convert to outs", {
  expect_equal(ip_to_outs(c("6.2", "0.1", "200.0", NA)), c(20L, 1L, 600L, NA))
})

test_that("stats parser yields one row per player-team-season with outs", {
  s <- parse_pitching_stats(read_fx("stats_synthetic.json"), 2030, "MLB")
  expect_equal(nrow(s), 3)
  expect_false(anyDuplicated(s[c("player_id", "team_id", "season")]) > 0)
  expect_equal(s$ip_outs[s$player_id == 7001], 541L)
  # no "outs" field: falls back to converting inningsPitched
  expect_equal(s$ip_outs[s$player_id == 7002 & s$team_id == 502], 62L)
  expect_true(all(c("age", "g", "gs", "tbf", "so", "bb", "hbp", "hr", "h", "r", "er", "sv", "w", "l", "level") %in% names(s)))
  expect_true(all(s$level == "MLB"))
})

test_that("transactions classify injured-list events; non-player rows are kept with NA id", {
  t <- parse_transactions(read_fx("transactions_synthetic.json"))
  expect_equal(nrow(t), 5)
  expect_equal(t$il_event, c("placed", "activated", "transferred", NA, NA))
  expect_true(is.na(t$player_id[t$txn_id == 5]))
  expect_s3_class(t$date, "Date")
})

test_that("monthly chunks cover the full range without gaps", {
  ch <- month_chunks(as.Date("2030-01-15"), as.Date("2030-03-10"))
  expect_equal(ch$start, as.Date(c("2030-01-15", "2030-02-01", "2030-03-01")))
  expect_equal(ch$end, as.Date(c("2030-01-31", "2030-02-28", "2030-03-10")))
})

test_that("a month with no transactions parses to an empty table with the full schema", {
  t <- parse_transactions(list(transactions = list()))
  expect_equal(nrow(t), 0)
  expect_true(all(c("txn_id", "date", "player_id", "team_id", "type_code", "description", "il_event") %in% names(t)))
})
