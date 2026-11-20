test_that("BIP fit recovers a linear relationship per count", {
  set.seed(1)
  d <- tibble::tibble(balls = 0L, strikes = 0L, in_play = 1L,
                      estimated_woba_using_speedangle = runif(500))
  d$delta_run_exp <- -0.25 + 1.2 * d$estimated_woba_using_speedangle + rnorm(500, 0, 0.01)
  b <- fit_bip_xrv(d)
  expect_equal(b$slope, 1.2, tolerance = 0.02)
})

test_that("target keeps non-BIP, replaces BIP, falls back to count mean", {
  bip <- tibble::tibble(balls = 0L, strikes = 0L, intercept = -0.2, slope = 1, bip_mean = 0.05)
  d <- tibble::tibble(balls = 0L, strikes = 0L, in_play = c(0L, 1L, 1L),
                      delta_run_exp = c(-0.04, 0.9, 0.9),
                      estimated_woba_using_speedangle = c(NA, 0.5, NA))
  expect_equal(denoised_target(d, bip), c(-0.04, 0.3, 0.05))
})
