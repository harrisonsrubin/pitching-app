rows_fx <- function() {
  ln <- function(pitcher, season, level = "MLB", tbf = 600, k = 150, age = 26)
    tibble::tibble(pitcher = as.integer(pitcher), season = as.integer(season), level = level, age = age,
                   age_missing = FALSE, tbf = tbf, k = k, bbhbp = 50, hr = 15, bip = tbf - k - 65, hbip = 120,
                   outs = 500, r = 70, er = 64, h = 135, bb = 45, hbp = 5, gs_share = 1)
  lines <- dplyr::bind_rows(
    ln(1, 2021, age = 25), ln(1, 2022, age = 26), ln(1, 2023, age = 27, k = 180), ln(1, 2024, age = 28, k = 200),
    ln(2, 2023, "AAA", tbf = 300, k = 90, age = 23), ln(2, 2024, tbf = 200, k = 40, age = 24),
    ln(3, 2023, tbf = 5, k = 0, age = 30), ln(3, 2024, tbf = 400, k = 80, age = 31),
    ln(9, 2020, age = 20), ln(9, 2021, age = 21), ln(9, 2022, age = 22), ln(9, 2023, age = 23), ln(9, 2024, age = 24))
  lines <- dplyr::bind_rows(lines, ln(99, 2023, "AAA", tbf = 20000, k = 4000), ln(98, 2024, "AAA", tbf = 20000, k = 4000))
  league <- league_table(lines)
  signals <- tibble::tibble(level = "MLB", season = 2023L, pitcher = 1L, stuff = 95, location = 50, pitching = 55,
                            x_whiff = 0.25, x_chase = 0.3, x_cstrike = 0.18, x_woba_contact = 0.36, x_gb = 0.45)
  windows <- tibble::tibble(level = "MLB", season = 2024L, pitcher = 1L, window = c("100", "to_date"),
                            tbf = c(25, 600), k = c(10, 200), bbhbp = c(2, 50), hr = c(1, 15), bip = c(12, 335),
                            hbip = c(4, 120), fb = c(4, 120), outs = c(18, 500), runs = c(3, 70),
                            stuff = c(70, 62), location = c(50, 50), pitching = c(55, 55))
  parks <- tibble::tibble(level = "MLB", season = 2023:2024, pitcher = 1L, ppf_runs = 1, ppf_hr = 1.25, ppf_hbip = 1)
  list(lines = lines, league = league, signals = signals, windows = windows, parks = parks)
}

test_that("full-season rows use only seasons up to the base season", {
  f <- rows_fx()
  r <- training_rows(f$lines, f$signals, f$windows, f$parks, f$league, horizon = 1L, component = "k")
  r1 <- r[r$pitcher == 1 & r$base_season == 2023 & r$window == "full", ]
  expect_equal(nrow(r1), 1)
  expect_equal(r1$target_season, 2024L)
  expect_equal(r1$events, 200); expect_equal(r1$trials, 600)
  changed <- f$lines; changed$k[changed$pitcher == 1 & changed$season == 2024] <- 10
  r2 <- training_rows(changed, f$signals, f$windows, f$parks, f$league, horizon = 1L, component = "k")
  preds <- setdiff(names(r), c("events", "trials", "lg_logit"))
  a <- r[r$base_season == 2023 & r$window == "full", preds]; b <- r2[r2$base_season == 2023 & r2$window == "full", preds]
  expect_equal(a[order(a$pitcher), ], b[order(b$pitcher), ])
})

test_that("grades enter clamped and scaled, and pitchers without grades are kept", {
  f <- rows_fx()
  r <- training_rows(f$lines, f$signals, f$windows, f$parks, f$league, horizon = 1L, component = "k")
  full <- r[r$window == "full", ]
  expect_equal(full$stuff[full$pitcher == 1 & full$base_season == 2023], 3)   # 95 -> clamped 80 -> (80-50)/10
  expect_equal(full$has_grades[full$pitcher == 1 & full$base_season == 2023], 1)
  p9 <- full[full$pitcher == 9 & full$base_season == 2023, ]
  expect_equal(p9$has_grades, 0); expect_equal(p9$stuff, 0)
})

test_that("Triple-A history counts, with its share recorded", {
  f <- rows_fx()
  r <- training_rows(f$lines, f$signals, f$windows, f$parks, f$league, horizon = 1L, component = "k")
  p2 <- r[r$pitcher == 2 & r$window == "full", ]
  expect_equal(p2$aaa_share, 1)
  expect_equal(p2$age_target, 24)
  expect_true(is.finite(p2$past_rel))
})

test_that("past results are finite with zero events, and home runs are park-adjusted", {
  f <- rows_fx()
  r <- training_rows(f$lines, f$signals, f$windows, f$parks, f$league, horizon = 1L, component = "k")
  expect_true(all(is.finite(r$past_rel)))
  h <- training_rows(f$lines, f$signals, f$windows, f$parks, f$league, horizon = 1L, component = "hr")
  expect_equal(h$events[h$pitcher == 1 & h$base_season == 2023 & h$window == "full"], 15 / 1.25)
})

test_that("rest-of-season rows target to-date minus the window", {
  f <- rows_fx()
  r <- training_rows(f$lines, f$signals, f$windows, f$parks, f$league, horizon = 0L, component = "k")
  w <- r[r$window == "100", ]
  expect_equal(nrow(w), 1)
  expect_equal(w$events, 190); expect_equal(w$trials, 575)
  expect_equal(w$stuff, 2)   # the window's own grade
})

test_that("newdata rows describe the latest season without a target", {
  f <- rows_fx()
  n <- newdata_rows(f$lines, f$signals, f$parks, f$league, base_season = 2024L, horizon = 1L, component = "k")
  expect_setequal(n$pitcher, c(1L, 2L, 3L, 9L, 98L))
  expect_false(any(c("events", "trials") %in% names(n)))
  expect_equal(n$age_target[n$pitcher == 1], 29)
})
