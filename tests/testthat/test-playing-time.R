# Injured-list days
il_txn <- function(player, date, event, desc = "x")
  tibble::tibble(txn_id = seq_along(date), date = as.Date(date), player_id = as.integer(player), team_id = 1L,
                 type_code = "SC", description = desc, il_event = event)

test_that("the old 'disabled list' name counts as the injured list", {
  expect_equal(classify_il("Boston Red Sox placed RHP Rick Porcello on the 10-day disabled list."), "placed")
  expect_equal(classify_il("Boston Red Sox activated RHP Rick Porcello from the 10-day disabled list."), "activated")
  expect_equal(classify_il("Yankees transferred RHP X from the 10-day disabled list to the 60-day disabled list."), "transferred")
})

test_that("injured-list days pair placements with activations, merge overlaps, and close open stints", {
  t <- dplyr::bind_rows(
    il_txn(1, c("2025-05-01", "2025-05-15", "2025-05-31"), c("placed", "transferred", "activated")),   # one 30-day stint
    il_txn(2, c("2025-08-02"), "placed"),                                                              # never activated
    il_txn(3, c("2025-03-01", "2025-04-09"), c("placed", "activated")))                                # starts before the season
  d <- il_days(t)
  expect_equal(d$il_days[d$pitcher == 1], 30)
  expect_equal(d$il_days[d$pitcher == 2], as.numeric(as.Date("2025-10-01") - as.Date("2025-08-02")))
  expect_equal(d$il_days[d$pitcher == 3], as.numeric(as.Date("2025-04-09") - as.Date("2025-03-20")))
  expect_true(all(d$season == 2025L))
})

# Playing-time rows
pt_fx <- function() {
  ln <- function(pitcher, season, level = "MLB", outs = 540, tbf = 750, gs_share = 1, age = 28, r = 70)
    tibble::tibble(pitcher = as.integer(pitcher), season = as.integer(season), level = level, age = age, age_missing = FALSE,
                   tbf = tbf, k = 0, bbhbp = 0, hr = 0, bip = 0, hbip = 0, outs = outs, r = r, er = r, h = 0, bb = 0, hbp = 0,
                   gs_share = gs_share)
  lines <- dplyr::bind_rows(
    ln(1, 2022), ln(1, 2023), ln(1, 2024), ln(1, 2025),
    ln(2, 2024, "AAA", outs = 300, tbf = 400), ln(2, 2025, "AAA", outs = 330, tbf = 420),
    ln(3, 2024, outs = 180, tbf = 240, gs_share = 0), ln(3, 2025, outs = 30, tbf = 45, gs_share = 0))
  il <- tibble::tibble(pitcher = 3L, season = 2025L, il_days = 195)
  profiles <- tibble::tibble(level = "MLB", season = c(2024L, 2025L), pitcher = 1L, pitch_type = "4S", n = 900,
                             velo = c(95.1, 94.0), arm_angle = c(40, 37))
  list(lines = lines, il = il, profiles = profiles, league = league_table(lines))
}

test_that("playing-time rows carry workload, health, trends and next-season targets", {
  f <- pt_fx()
  r <- pt_rows(f$lines, f$il, f$profiles, f$league, base_seasons = 2024L)
  p1 <- r[r$pitcher == 1, ]
  expect_equal(p1$outs_1, 540); expect_equal(p1$outs_2, 540); expect_equal(p1$max_outs3, 540)
  expect_equal(p1$available, 1); expect_equal(p1$starter, 1); expect_equal(p1$outs_next, 540)
  expect_equal(p1$age_next, 29)
  p2 <- r[r$pitcher == 2, ]
  expect_equal(p2$aaa_share, 1); expect_equal(p2$outs_1, 0); expect_equal(p2$available, 0); expect_true(is.na(p2$starter))
  r25 <- pt_rows(f$lines, f$il, f$profiles, f$league, base_seasons = 2025L, with_target = FALSE)
  expect_equal(r25$velo_chg[r25$pitcher == 1], 94.0 - 95.1)
  expect_equal(r25$has_velo[r25$pitcher == 3], 0); expect_equal(r25$velo_chg[r25$pitcher == 3], 0)
  expect_equal(r25$il_1[r25$pitcher == 3], 195)
  expect_false(any(c("available", "starter", "outs_next") %in% names(r25)))
  expect_true(all(is.finite(r25$perf_ra9)))
})

# Simulated playing-time rows with known structure
pt_sim <- function(n = 3000, seed = 4) {
  set.seed(seed)
  outs_1 <- sample(c(0, 60, 150, 300, 540), n, TRUE); gs <- as.numeric(outs_1 >= 300) * runif(n, 0.6, 1)
  il_1 <- ifelse(runif(n) < 0.2, runif(n, 20, 190), 0); aaa <- ifelse(outs_1 == 0, 1, 0)
  age <- sample(22:38, n, TRUE); perf <- rnorm(n, 4.4, 0.8)
  p_av <- plogis(-0.5 + 0.004 * outs_1 - 0.01 * il_1 - 0.08 * (age - 28) * (age > 28) - 1.5 * aaa)
  avail <- rbinom(n, 1, p_av)
  starter <- ifelse(avail == 1, rbinom(n, 1, plogis(-3 + 6 * gs)), NA)
  outs_next <- ifelse(avail == 1, pmin(round(exp(log(pmax(outs_1, 30) * ifelse(starter == 1, 1.1, 0.8)) + rnorm(n, 0, 0.5))), 690), 0)
  tibble::tibble(pitcher = seq_len(n), base_season = 2020L, outs_1 = outs_1, outs_2 = outs_1, outs_3 = outs_1, max_outs3 = outs_1,
                 gs_share = gs, aaa_share = aaa, il_1 = il_1, il_3 = il_1, perf_ra9 = perf, has_velo = 0, velo_chg = 0, arm_chg = 0,
                 age_next = age, available = avail, starter = starter, outs_next = outs_next)
}

test_that("playing-time models learn workload, injury and role effects, quickly", {
  r <- pt_sim()
  t0 <- Sys.time(); pt <- fit_playing_time(r); expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 10)
  base <- r[1, ]; base$outs_1 <- base$outs_2 <- base$outs_3 <- base$max_outs3 <- 300; base$il_1 <- base$il_3 <- 0; base$aaa_share <- 0; base$age_next <- 28
  hurt <- base; hurt$il_1 <- hurt$il_3 <- 150
  expect_lt(stats::predict(pt$avail, hurt, type = "response"), stats::predict(pt$avail, base, type = "response"))
  big <- base; big$outs_1 <- big$outs_2 <- big$outs_3 <- big$max_outs3 <- 540
  nd <- function(x, s) { x$starter <- s; x }
  expect_gt(stats::predict(pt$ip, nd(big, 1)), stats::predict(pt$ip, nd(base, 1)))
  expect_true(all(pt$sigma > 0))
})

test_that("simulated innings are bounded, include lost seasons, and favor established starters", {
  r <- pt_sim(); pt <- fit_playing_time(r)
  nd <- r[1:3, ]
  nd[1, c("outs_1", "outs_2", "outs_3", "max_outs3")] <- 540; nd$gs_share[1] <- 1; nd$il_1[1] <- 0; nd$aaa_share[1] <- 0; nd$age_next[1] <- 28
  nd[2, c("outs_1", "outs_2", "outs_3", "max_outs3")] <- 0; nd$aaa_share[2] <- 1; nd$gs_share[2] <- 1; nd$il_1[2] <- 0
  nd$outs_1[3] <- 0; nd$outs_2[3] <- 300; nd$outs_3[3] <- 300; nd$max_outs3[3] <- 300
  nd$il_1[3] <- 195; nd$il_3[3] <- 195; nd$aaa_share[3] <- 0
  s <- simulate_playing_time(pt, nd, n_draws = 2000L, horizons = 3L)
  expect_length(s, 3)
  expect_equal(dim(s[[1]]$outs), c(3L, 2000L))
  expect_true(all(unlist(lapply(s, function(x) x$outs)) >= 0 & unlist(lapply(s, function(x) x$outs)) <= 690))
  p_mlb <- rowMeans(s[[1]]$outs > 0)
  expect_lt(p_mlb[2], p_mlb[1])
  expect_gt(mean(s[[1]]$outs[3, ] == 0), 0.2)
})

# WAR
test_that("runs per win follows the run environment", {
  expect_equal(runs_per_win(4.5), 1.5 * 4.5 + 3)
})

test_that("replacement levels are scaled so realized pitcher WAR totals 430 a season", {
  set.seed(9)
  act <- tibble::tibble(pitcher = 1:600, season = 2025L, starter = rep(c(1, 0), each = 300),
                        outs = ifelse(rep(c(1, 0), each = 300) == 1, runif(600, 300, 600), runif(600, 60, 200)),
                        ra9_adj = rnorm(600, 4.5, 1))
  league <- tibble::tibble(season = 2025L, level = "MLB", lg_ra9 = 4.5)
  k <- replacement_scale(act, league, 2025L)
  w <- war_from(act$ra9_adj, act$outs, act$starter, lg_ra9 = 4.5, k = k)
  expect_equal(sum(w), 430, tolerance = 1e-6)
})

test_that("a replacement-level pitcher is worth 0, and no innings means 0 WAR, not NaN", {
  k <- 1
  repl_sp <- 4.5 + k * (0.5 - REPL_WIN[["sp"]]) * runs_per_win(4.5)
  expect_equal(war_from(repl_sp, 540, 1, 4.5, k), 0)
  expect_equal(war_from(3.0, 0, 1, 4.5, k), 0)
  expect_gt(war_from(3.0, 540, 1, 4.5, k), war_from(3.0, 540, 0, 4.5, k))   # starters face a lower replacement bar
})

test_that("innings draws reuse the real, lopsided spread of past misses by role", {
  r <- pt_sim()
  pl <- r$available == 1 & !is.na(r$starter)
  r$outs_next[pl] <- pmax(1, round(r$outs_next[pl] * ifelse(runif(sum(pl)) < 0.2, 0.2, 1)))   # injuries only cut innings
  pt <- fit_playing_time(r)
  expect_true(all(c("rp", "sp") %in% names(pt$resid)))
  nd <- r[which(r$outs_1 == 540)[1], ]; nd$il_1 <- 0; nd$il_3 <- 0
  s <- simulate_playing_time(pt, nd, n_draws = 4000L, horizons = 1L)[[1]]
  o <- s$outs[s$outs > 0]
  mu <- stats::predict(pt$ip, transform(nd, starter = 1))
  expect_lte(stats::quantile(o, 0.95), exp(mu + max(unlist(pt$resid$sp))) + 1)
  cap_in_data <- mean(r$outs_next[pl & r$outs_1 == 540 & r$starter == 1] >= PT_MAX_OUTS)
  expect_lte(mean(o == PT_MAX_OUTS), cap_in_data + 0.03)   # no more capped seasons than the data shows
})

test_that("misses are grouped by projected innings, so heavy projections get their own (smaller) upside", {
  r <- pt_sim(); pt <- fit_playing_time(r)
  expect_length(pt$resid$sp, PT_BANDS); expect_length(pt$breaks$sp, PT_BANDS - 1)
  expect_lt(stats::quantile(pt$resid$sp[[PT_BANDS]], 0.95), stats::quantile(pt$resid$sp[[1]], 0.95))
})

test_that("in-sample expected innings match actual innings for pitchers who pitched", {
  r <- pt_sim(); r$outs_next[r$available == 1] <- pmax(1, round(exp(log(r$outs_next[r$available == 1]) + rnorm(sum(r$available == 1), 0, 0.3) * (r$outs_1[r$available == 1] < 200))))
  pt <- fit_playing_time(r)
  pl <- r[r$available == 1 & !is.na(r$starter), ]
  s <- simulate_playing_time(pt, pl, 300L, 1L)[[1]]
  sim_given <- mean(rowSums(s$outs) / pmax(rowSums(s$outs > 0), 1))  # each pitcher's innings when he pitches
  expect_lt(abs(sim_given / mean(pl$outs_next) - 1), 0.04)
})

test_that("the 60-game 2020 season is scaled to a full season, and 2020 is never a target", {
  f <- pt_fx()
  f$lines <- dplyr::bind_rows(f$lines, dplyr::mutate(f$lines[f$lines$pitcher == 1 & f$lines$season == 2022, ], season = 2020L, outs = 200, r = 25),
                              dplyr::mutate(f$lines[f$lines$pitcher == 1 & f$lines$season == 2022, ], season = 2019L))
  r <- pt_rows(f$lines, f$il, f$profiles, f$league, base_seasons = 2020L, with_target = FALSE)
  expect_equal(r$outs_1[r$pitcher == 1], round(200 * 162 / 60))
  expect_equal(nrow(pt_rows(f$lines, f$il, f$profiles, f$league, base_seasons = 2019L)), 0)
})

test_that("an open injured-list stint closes at its own season's end, not later", {
  t <- il_txn(5, c("2024-07-01", "2025-04-01", "2025-06-01"), c("placed", "placed", "activated"))
  d <- il_days(t)
  expect_equal(d$il_days[d$season == 2024], as.numeric(as.Date("2024-10-01") - as.Date("2024-07-01")))
  expect_equal(d$il_days[d$season == 2025], 61)
})

test_that("mid-season, playing time trains and calibrates on completed seasons only", {
  expect_false(season_complete(2026L, as.Date("2026-06-15")))
  expect_true(season_complete(2026L, as.Date("2026-11-21")))
  s <- pt_setup(2026L, today = as.Date("2026-06-15"))
  expect_equal(s$base, 2025L); expect_equal(s$shift, 1L); expect_equal(max(s$train), 2024L); expect_equal(max(s$war), 2025L)
  s2 <- pt_setup(2026L, today = as.Date("2026-11-21"))
  expect_equal(s2$base, 2026L); expect_equal(s2$shift, 0L); expect_equal(max(s2$train), 2025L); expect_equal(max(s2$war), 2026L)
})

test_that("a pitcher who lost the whole base season is still in the rows and the projection", {
  f <- pt_fx()
  lost <- f$lines[f$lines$pitcher == 1 & f$lines$season %in% 2022:2023, ]; lost$pitcher <- 9L
  f$lines <- dplyr::bind_rows(f$lines, lost)
  f$il <- dplyr::bind_rows(f$il, tibble::tibble(pitcher = 9L, season = 2024L, il_days = 195))
  r <- pt_rows(f$lines, f$il, f$profiles, f$league, base_seasons = 2024L)
  p9 <- r[r$pitcher == 9, ]
  expect_equal(nrow(p9), 1); expect_equal(p9$outs_1, 0); expect_equal(p9$outs_2, 540); expect_equal(p9$il_1, 195)
  expect_equal(p9$age_next, 30); expect_equal(p9$available, 0)
  nd <- newdata_rows(f$lines, ensure_signal_cols(tibble::tibble(level = character(), season = integer(), pitcher = integer())),
                     NULL, f$league, base_season = 2024L, horizon = 1L, component = "k")
  expect_true(9L %in% nd$pitcher)
  expect_true(is.finite(nd$age_target[nd$pitcher == 9]))
})

test_that("innings move with each draw's talent: worse draws pitch less", {
  r <- pt_sim(); r$available <- rbinom(nrow(r), 1, plogis(qlogis(pmin(pmax(r$available * 0.9 + 0.05, 0.01), 0.99)) - 0.8 * (r$perf_ra9 - 4.4)))
  r$starter[r$available == 0] <- NA; r$outs_next[r$available == 0] <- 0
  pt <- fit_playing_time(r)
  nd <- r[rep(which(r$outs_1 == 300)[1], 2), ]; nd$pitcher <- 1:2
  perf <- list(matrix(c(rep(3.0, 3000), rep(6.0, 3000)), 2, 3000, byrow = TRUE))
  s <- simulate_playing_time(pt, nd, n_draws = 3000L, horizons = 1L, perf = perf)[[1]]
  expect_gt(mean(s$outs[1, ]), mean(s$outs[2, ]))
})

test_that("talent draws shift playing time around the pitcher's own past level, not to a new level", {
  base <- matrix(c(4.0, 4.2, 3.8), 1, 3); past <- 4.6
  expect_equal(perf_from_draws(base, past), matrix(past + c(4.0, 4.2, 3.8) - 4.0, 1, 3))
})

test_that("recent seasons count more, so a league-wide drop in innings per pitcher is followed", {
  old <- pt_sim(seed = 1); old$base_season <- 2015L
  new <- pt_sim(seed = 2); new$base_season <- 2023L; new$outs_next <- round(new$outs_next * 0.7)
  pt <- fit_playing_time(dplyr::bind_rows(old, new))
  nd <- new[new$available == 1 & !is.na(new$starter), ]
  pred <- mean(exp(stats::predict(pt$ip, nd)))
  expect_lt(abs(pred / mean(nd$outs_next) - 1), abs(pred / mean(c(nd$outs_next, nd$outs_next / 0.7)) - 1))
})
