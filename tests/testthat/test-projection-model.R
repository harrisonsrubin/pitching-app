sim_rows <- function(n = 1500, seed = 11) {
  set.seed(seed)
  u <- rnorm(n, 0, 0.3)
  big <- rep(c(TRUE, FALSE), length.out = n)
  rows <- lapply(1:4, function(s) {
    tr_past <- ifelse(big, 600, 60)
    past_ev <- rbinom(n, tr_past, plogis(-1.4 + u))
    lgp <- plogis(-1.4)
    past_rel <- qlogis((past_ev + lgp) / (tr_past + 1)) - qlogis(lgp)
    has <- as.numeric(runif(n) < 0.7)
    stuff <- ifelse(has == 1, (u / 0.3 + rnorm(n, 0, 0.7)) * 0.8, 0)
    tibble::tibble(pitcher = seq_len(n), base_season = 2019L + s, target_season = 2020L + s, window = "full", horizon = 1L,
                   trials = 500, events = rbinom(n, 500, plogis(-1.4 + u + rnorm(n, 0, 0.05))),
                   lg_logit = -1.4, past_rel = past_rel, past_trials = tr_past, has_grades = has,
                   stuff = stuff, location = 0, pitching = stuff, x_whiff = 0, x_chase = 0, x_cstrike = 0,
                   x_woba_contact = 0, x_gb = 0, age_target = sample(22:36, n, TRUE), gs_share = 0.5, aaa_share = 0)
  })
  dplyr::bind_rows(rows)
}

test_that("results count for more when they come from a bigger sample", {
  r <- sim_rows()
  t0 <- Sys.time(); m <- fit_component(r, "k"); secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_lt(secs, 120)
  nd <- r[c(1, 2), ]; nd$has_grades <- 0; nd$stuff <- 0; nd$pitching <- 0; nd$pitcher <- c(-1L, -2L)
  nd$past_trials <- c(600, 60); nd$past_rel <- 0
  up <- nd; up$past_rel <- 0.3
  move <- predict_component(m, up) - predict_component(m, nd)
  expect_gt(move[1], move[2])
  expect_gt(move[2], 0)
})

test_that("a pitcher without grades still gets a finite prediction, and the stuff weight is a share", {
  r <- sim_rows(n = 600)
  m <- fit_component(r, "k")
  nd <- r[1:5, ]; nd$has_grades <- c(0, 1, 1, 0, 1); nd$stuff[nd$has_grades == 0] <- 0
  p <- predict_component(m, nd)
  expect_true(all(is.finite(p)))
  w <- stuff_weight(m, nd)
  expect_true(all(w >= 0 & w <= 1))
  expect_equal(w[nd$has_grades == 0], c(0, 0))
})

test_that("the season-change variance is available for simulation", {
  m <- fit_component(sim_rows(n = 600), "k")
  v <- change_variance(m)
  expect_true(is.finite(v) && v >= 0)
})

test_that("talent uncertainty shrinks as the past sample grows", {
  m <- fit_component(sim_rows(n = 1500), "k")
  v <- talent_uncertainty(m, c(30, 300, 3000))
  expect_true(all(v >= 0))
  expect_gt(v[1], v[2]); expect_gte(v[2], v[3])
})

test_that("a model trained on a single season still predicts", {
  r <- sim_rows(n = 400); r <- r[r$target_season == 2021L, ]
  m <- fit_component(r, "k")
  nd <- r[1:3, ]; nd$target_season <- 2030L
  expect_true(all(is.finite(predict_component(m, nd))))
})

test_that("the stuff weight measures movement away from the typical pitcher, by source", {
  r <- sim_rows(n = 800); m <- fit_component(r, "k")
  base <- r[1, ]; base$pitcher <- -9L
  for (v in c("past_rel", "past_trials", "has_grades", "stuff", "pitching", "age_target")) base[[v]] <- mean(r[[v]])
  base$past_trials <- exp(mean(log1p(r$past_trials))) - 1
  expect_equal(stuff_weight(m, base), 0, tolerance = 0.05)
  s_only <- base; s_only$stuff <- base$stuff + 2; s_only$pitching <- base$pitching + 2
  expect_gt(stuff_weight(m, s_only), 0.9)
  r_only <- base; r_only$past_rel <- base$past_rel + 0.4
  expect_lt(stuff_weight(m, r_only), 0.1)
})
