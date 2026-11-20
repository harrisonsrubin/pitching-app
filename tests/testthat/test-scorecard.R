test_that("weighted correlation equals Pearson with equal weights", {
  set.seed(5); x <- rnorm(50); y <- x + rnorm(50)
  expect_equal(wcor(x, y, rep(1, 50)), cor(x, y))
  expect_equal(wspearman(x, y, rep(1, 50)), cor(x, y, method = "spearman"))
})

test_that("season pairs use both-season qualification and the smaller IP as weight", {
  m <- tibble::tibble(pitcher = c(1L, 1L, 2L, 2L), season = c(2030L, 2031L, 2030L, 2031L), value = c(1, 2, 3, 4))
  ip <- tibble::tibble(pitcher = c(1L, 1L, 2L, 2L), season = c(2030L, 2031L, 2030L, 2031L), ip = c(160, 120, 160, 30))
  p <- pair_seasons(m, ip)
  expect_equal(p$pitcher, 1L); expect_equal(p$w, 120); expect_equal(p$band, "100-150")
  expect_equal(disappeared_count(m, ip)$n, 1L)
})

test_that("IP bands", {
  expect_equal(ip_band(c(40, 50, 99.9, 100, 150, 200)), c(NA, "50-100", "50-100", "100-150", "150+", "150+"))
})

test_that("CV never fits on the held-out pitcher", {
  df <- tibble::tibble(pitcher = 1:10, x = 1:10, y = c(1:9, 100), w = 1)
  r <- cv_rmse(df, y ~ x, df$w, folds = rep(1:2, 5))
  # pitcher 10 (y = 100) is in fold 2; fold 1's fit (y = x exactly) predicts 10 for him -> error 90
  expect_gt(r$rmse, 10)
})

test_that("CV with default folds tolerates a missing predictor", {
  df <- tibble::tibble(pitcher = 1:20, x = c(NA, 2:20), y = 1:20 + 0.1, w = 1)
  expect_silent(r <- cv_rmse(df, y ~ x, df$w))
  expect_equal(r$n, 19L)
})
