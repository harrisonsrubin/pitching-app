world_projection <- function() {
  lines <- sim_world(); league <- league_table(lines)
  signals <- tibble::tibble(level = character(), season = integer(), pitcher = integer())
  models <- fit_projection_models(lines, signals, windows = NULL, parks = NULL, league = league, horizons = 1:2)
  map <- fit_runs_map(lines, parks = NULL)
  anchors <- tibble::tibble(mean = 4.3, unit = 0.6)
  curves <- stats::setNames(lapply(PROJ_COMPONENTS, function(cpt) tibble::tibble(age = AGE_RANGE, cum = 0)), PROJ_COMPONENTS)
  project_pitchers(models, lines, signals, NULL, league, base_season = 2024L, map = map, anchors = anchors,
                   curves = curves, n_draws = 300L, seed = 1L)
}

test_that("chips are ten non-decreasing values inside 20-80", {
  ch <- chips(c(rnorm(500, 50, 20), 150, -40))
  expect_length(ch, 10)
  expect_true(all(diff(ch) >= 0)); expect_true(all(ch >= 20 & ch <= 80))
})

test_that("the true talent grade runs the right way and is capped", {
  a <- tibble::tibble(mean = 4.3, unit = 0.6)
  expect_equal(tt_grade(4.3, a), 50); expect_equal(tt_grade(3.7, a), 60)
  expect_equal(tt_grade(c(0, 12), a), c(80, 20))
})

test_that("step odds measure change: none without change, symmetric with large change", {
  set.seed(2)
  now <- rnorm(2000, 4, 0.3)
  expect_equal(unname(step_odds(now, now)), c(0, 0))
  nxt <- now + rnorm(2000, 0, 1)
  o <- step_odds(now, nxt)
  expect_gt(o[["forward"]], 0.15); expect_equal(o[["forward"]], o[["back"]], tolerance = 0.25)
})

test_that("basis is MLB-equivalent when most recent batters faced were in Triple-A", {
  l <- tibble::tibble(pitcher = c(1L, 1L, 2L), season = 2024L, level = c("AAA", "MLB", "MLB"), tbf = c(300, 100, 500))
  b <- basis_of(l, 2024L)
  expect_equal(b$basis[b$pitcher == 1], "MLB-equivalent"); expect_equal(b$basis[b$pitcher == 2], "MLB")
})

test_that("projections: a tiny-sample rookie is finite with a wider range than a full-time veteran", {
  p <- world_projection()
  h1 <- p[p$horizon == 1, ]
  expect_true(all(is.finite(h1$era) & h1$era > 0))
  expect_true(all(c(paste0("chip_", 1:10)) %in% names(p)))
  rk <- h1[h1$pitcher == 9999, ]; vet <- h1[h1$pitcher == 1, ]
  expect_gt(rk$era_hi - rk$era_lo, vet$era_hi - vet$era_lo)
  expect_true(all(unlist(rk[paste0("chip_", 1:10)]) >= 20 & unlist(rk[paste0("chip_", 1:10)]) <= 80))
  expect_equal(h1$basis[h1$pitcher == 8888], "MLB-equivalent")
  expect_setequal(unique(p$horizon), 1:5)
  expect_true(all(is.na(p$step_forward[p$horizon != 1])))
  expect_true(all(h1$step_forward >= 0 & h1$step_forward <= 1))
  expect_true(all(h1$era_out_hi - h1$era_out_lo >= h1$era_hi - h1$era_lo))
})

test_that("today and next season share each draw's talent, so the odds reflect change only", {
  p <- world_projection()
  h1 <- p[p$horizon == 1, ]
  expect_lt(stats::median(h1$step_forward + h1$step_back), 0.5)
})

test_that("point estimates of today's talent RA9 feed the frozen grade anchors", {
  lines <- sim_world(); league <- league_table(lines)
  sig <- tibble::tibble(level = character(), season = integer(), pitcher = integer())
  models <- fit_projection_models(lines, sig, windows = NULL, parks = NULL, league = league, horizons = 1L)
  map <- fit_runs_map(lines, parks = NULL)
  tp <- dplyr::mutate(talent_point(models, lines, sig, NULL, league, 2023L, map), season = 2023L)
  expect_true(all(is.finite(tp$ra9)))
  a <- projection_anchors(tp, lines, 2023L)
  expect_true(a$unit > 0 && is.finite(a$mean))
})

test_that("next-season draws can be kept for the scorecard", {
  lines <- sim_world(); league <- league_table(lines)
  sig <- tibble::tibble(level = character(), season = integer(), pitcher = integer())
  models <- fit_projection_models(lines, sig, windows = NULL, parks = NULL, league = league, horizons = 1:2)
  curves <- stats::setNames(lapply(PROJ_COMPONENTS, function(cpt) tibble::tibble(age = AGE_RANGE, cum = 0)), PROJ_COMPONENTS)
  p <- project_pitchers(models, lines, sig, NULL, league, 2024L, fit_runs_map(lines, NULL), tibble::tibble(mean = 4.3, unit = 0.6),
                        curves, n_draws = 50L, keep_draws = TRUE)
  d <- attr(p, "draws")
  expect_equal(dim(d$era_out), c(length(unique(p$pitcher)), 50L))
  expect_equal(rownames(d$era_out), as.character(unique(p$pitcher)))
})

test_that("season-outcome noise includes the runs map's own error, shrinking with innings", {
  lines <- sim_world(); map <- fit_runs_map(lines, NULL)
  s <- runs_noise_sd(map, c(150, 600))
  expect_true(all(s > 0)); expect_gt(s[1], s[2])
  expect_equal(s[1] / s[2], 2, tolerance = 1e-9)
})

test_that("the forecasting fit uses only target seasons with park factors (2020 on); older seasons feed aging", {
  lines <- sim_world(); league <- league_table(lines)
  sig <- tibble::tibble(level = character(), season = integer(), pitcher = integer())
  models <- fit_projection_models(lines, sig, windows = NULL, parks = NULL, league = league, horizons = 1L)
  seasons <- as.integer(sub(".*_", "", unique(lme4::getME(models$k$h1$fit, "flist")$pitcher_season)))
  expect_gte(min(seasons), PROJ_MIN_TARGET)
})

test_that("season 2 can be stepped from next season with the aging curve (no jump without aging)", {
  lines <- sim_world(); league <- league_table(lines)
  sig <- tibble::tibble(level = character(), season = integer(), pitcher = integer())
  models <- fit_projection_models(lines, sig, windows = NULL, parks = NULL, league = league, horizons = 1:2)
  curves <- stats::setNames(lapply(PROJ_COMPONENTS, function(cpt) tibble::tibble(age = AGE_RANGE, cum = 0)), PROJ_COMPONENTS)
  p <- project_pitchers(models, lines, sig, NULL, league, 2024L, fit_runs_map(lines, NULL), tibble::tibble(mean = 4.3, unit = 0.6),
                        curves, n_draws = 400L, h2_method = "step")
  a <- p$era[p$horizon == 1]; b <- p$era[p$horizon == 2]
  expect_lt(abs(stats::median(b - a)), 0.05)
})
