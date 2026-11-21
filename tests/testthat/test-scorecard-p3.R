test_that("interval coverage counts actuals inside their ranges", {
  expect_equal(coverage(c(0, 0), c(1, 1), c(0.5, 2)), 0.5)
  expect_true(is.na(coverage(numeric(), numeric(), numeric())))
})

test_that("sample CRPS is 0 for draws all at the actual and grows with distance", {
  expect_equal(crps_draws(rep(3, 50), 3), 0)
  expect_gt(crps_draws(rep(3, 50), 4), crps_draws(rep(3, 50), 3.5))
  expect_equal(crps_draws(rep(3, 50), 4), 1)
})

test_that("step calibration bins predicted odds and counts realized jumps", {
  pred <- c(0.02, 0.04, 0.12, 0.18, 0.31); real <- c(FALSE, TRUE, FALSE, TRUE, TRUE)
  cal <- step_calibration(pred, real, breaks = c(0, 0.1, 0.2, 1))
  expect_equal(cal$n, c(2, 2, 1))
  expect_equal(cal$observed, c(0.5, 0.5, 1))
  expect_equal(cal$predicted, c(0.03, 0.15, 0.31))
})
