toy_pa <- function() {
  tibble::tibble(game_pk = 1L, at_bat_number = 1:10, pitcher = 7L, season = 2025L,
                 game_date = as.Date("2025-04-01"), inning = 1L, inning_topbot = "Top",
                 outs = c(1L, 0L, 0L, 0L, 1L, 0L, 1L, 1L, 0L, 0L), runs = c(0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 0L),
                 events = c("strikeout", "walk", "home_run", "hit_by_pitch", "field_out", "single", "strikeout",
                            "field_out", "double", "intent_walk"),
                 bb_type = c(NA, NA, "fly_ball", NA, "fly_ball", "line_drive", NA, "ground_ball", "fly_ball", NA)) %>%
    dplyr::mutate(key = order_key(game_date, game_pk, at_bat_number))
}

test_that("window counts tally the four components, fly balls, outs and runs", {
  w <- window_counts(toy_pa())
  expect_equal(w$tbf, 10); expect_equal(w$k, 2); expect_equal(w$bbhbp, 3); expect_equal(w$hr, 1)
  expect_equal(w$bip, 10 - 2 - 3 - 1); expect_equal(w$hbip, 2); expect_equal(w$fb, 3)
  expect_equal(w$outs, 4); expect_equal(w$runs, 2)
})

test_that("a cutoff keeps only plate appearances at or before it", {
  pa <- toy_pa()
  w <- window_counts(pa, tibble::tibble(pitcher = 7L, cutoff = pa$key[4]))
  expect_equal(w$tbf, 4); expect_equal(w$k, 1); expect_equal(w$bbhbp, 2); expect_equal(w$hr, 1)
  expect_equal(w$bip, 0); expect_equal(w$hbip, 0)
  expect_false(any(is.nan(unlist(w))))
})

test_that("component park factors are half-regressed home-vs-road ratios", {
  # Team A's home park allows twice the HR rate its hitters/pitchers see on the road.
  sums <- dplyr::bind_rows(
    tibble::tibble(season = 2025L, venue_id = 1L, team = "A", side = "home", pa = 1000, hr = 40, bip = 700, hbip = 210, runs = 500, games = 81),
    tibble::tibble(season = 2025L, venue_id = NA_integer_, team = "A", side = "road", pa = 1000, hr = 20, bip = 700, hbip = 210, runs = 500, games = 81))
  pf <- component_park_factors(sums)
  expect_equal(pf$pf_hr[pf$venue_id == 1], 1.5)
  expect_equal(pf$pf_hbip[pf$venue_id == 1], 1)
  expect_equal(pf$pf_runs[pf$venue_id == 1], 1)
})

test_that("park sums split home and road by team, and pitcher parks weight by plate appearances", {
  pa <- toy_pa(); pa$game_pk <- rep(c(1L, 2L), each = 5)
  scores <- tibble::tibble(game_pk = c(1L, 2L), season = 2025L, venue_id = c(1L, 2L), home_team = c("A", "B"),
                           away_team = c("B", "A"), home_runs = 3, away_runs = 2)
  s <- park_sums(pa, scores)
  expect_setequal(unique(s$side), c("home", "road"))
  expect_equal(sum(s$pa[s$side == "home"]), 10)
  cpf <- tibble::tibble(venue_id = c(1L, 2L), season = 2025L, pf_runs = c(1.2, 1), pf_hr = c(1.4, 1), pf_hbip = c(1, 1))
  pp <- pitcher_parks(pa, scores, cpf)
  expect_equal(pp$ppf_hr, mean(c(1.4, 1)))
  pp2 <- pitcher_parks(pa, scores, cpf[1, ])          # an unknown park counts as 1
  expect_equal(pp2$ppf_runs, mean(c(1.2, 1)))
})

test_that("outputs include windows and parks, still nothing pitch-level", {
  k <- outputs_for("MLB", 2026L)
  expect_length(k, 11)
  expect_true(all(c("outputs/windows_MLB_2026.parquet", "outputs/parks_MLB_2026.parquet") %in% k))
  expect_true(all(startsWith(k, "outputs/")))
})
