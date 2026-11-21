test_that("folds are deterministic and in range", {
  expect_equal(fold_of(c(5L, 6L, 11L)), c(1L, 2L, 2L))
  expect_true(all(fold_of(sample(1e6, 1000)) %in% 1:5))
})

test_that("recency weights", {
  expect_equal(recency_weight(c(2026, 2025, 2023), 2026), c(1, 0.8, 0.512))
})

test_that("lightgbm learns a monotone signal", {
  set.seed(3); X <- matrix(runif(4000), ncol = 2); y <- 2 * X[, 1] + rnorm(2000, 0, 0.1)
  m <- train_lgb(X, y, rep(1, 2000), "regression", valid = seq_len(2000) %% 10 == 0)
  expect_gt(cor(predict(m, X), y), 0.9)
})

test_that("each pitch is scored by the model that never saw its pitcher", {
  boosters <- lapply(1:5, function(k) structure(list(k = k), class = "fake_booster"))
  local_global_binding(predict_booster = function(b, X) rep(b$k, nrow(X)))
  d <- tibble::tibble(pitcher = c(5L, 6L, 7L), a = 0)
  expect_equal(predict_by_fold(d, boosters, feats = "a"), c(1, 2, 3))
})

test_that("stuff is averaged over the count mix", {
  mix <- tibble::tibble(balls = c(0L, 3L), strikes = c(0L, 2L), same_side = 0L, share = c(0.75, 0.25))
  f <- function(X) X[, "balls"]  # prediction = balls
  expect_equal(neutral_count_predict(tibble::tibble(balls = 1L, strikes = 1L, same_side = 1L), f, mix), 0.75)
})

test_that("position players are not training rows", {
  d <- tibble::tibble(pitcher = c(rep(1L, 150), rep(2L, 150)), season = 2030L, level = "MLB",
                      pitch_type = "4S", velo = c(rep(94, 150), rep(70, 150)))
  expect_equal(unique(d$pitcher[training_eligible(d)]), 1L)
})
