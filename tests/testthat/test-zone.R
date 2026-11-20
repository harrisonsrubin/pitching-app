t_cross <- function(vy0, ay, y) { a <- ay / 2; c <- 50 - y; (-vy0 - sqrt(vy0^2 - 4 * a * c)) / (2 * a) }

test_that("mid-plate locations convert to the front of the plate", {
  p <- list(vx0 = 5, vy0 = -130, vz0 = -5, ax = -10, ay = 28, az = -15)
  tf <- t_cross(p$vy0, p$ay, 17 / 12); tm <- t_cross(p$vy0, p$ay, 17 / 24)
  z <- function(t) 6 + p$vz0 * t + p$az * t^2 / 2
  x <- function(t) 0.5 + p$vx0 * t + p$ax * t^2 / 2
  d <- tibble::as_tibble(p) %>% dplyr::mutate(plate_x = x(tm), plate_z = z(tm))
  out <- to_front_plane(d)
  expect_equal(out$plate_z, z(tf), tolerance = 1e-9)
  expect_equal(out$plate_x, x(tf), tolerance = 1e-9)
})

test_that("ABS-era data is MLB 2026 on and Triple-A 2023 on", {
  expect_true(is_abs_era("MLB", 2027L)); expect_true(is_abs_era("AAA", 2023L))
  expect_true(is_abs_era("MLB", 2026)); expect_false(is_abs_era("MLB", 2025))
  expect_true(all(sapply(2023:2026, function(s) is_abs_era("AAA", s))))
})

test_that("zones: posture seasons use the batter's median; ABS seasons are mapped to the posture scale", {
  zm <- tibble::tibble(edge = c("top", "bot"), intercept = c(0.5, 0.1), slope = c(1, 1))
  pos <- tibble::tibble(batter = c(1L, 1L, 2L), sz_top = c(3.3, 3.5, 3.0), sz_bot = c(1.5, 1.7, 1.6))
  out <- harmonize_zone(pos, zm, abs_era = FALSE)
  expect_equal(out$sz_top, c(3.4, 3.4, 3.0)); expect_equal(out$sz_bot, c(1.6, 1.6, 1.6))
  abs <- tibble::tibble(batter = 1L, sz_top = 3.0, sz_bot = 1.5)
  out2 <- harmonize_zone(abs, zm, abs_era = TRUE)
  expect_equal(out2$sz_top, 3.5); expect_equal(out2$sz_bot, 1.6)
})
