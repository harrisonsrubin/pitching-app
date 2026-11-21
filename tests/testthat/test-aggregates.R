zone_px <- function(m) m * 0.83   # horizontal distance giving max(u, v) = m at mid height

test_that("attack zones follow the 67/133/200% boundaries of the batter's zone", {
  z <- attack_zone(zone_px(c(0.66, 0.67, 1.32, 1.33, 1.99, 2, 0)), 2.5, 3.5, 1.5)
  expect_equal(z, c("Heart", "Shadow", "Shadow", "Chase", "Chase", "Waste", "Heart"))
  expect_equal(attack_zone(0, 3.5, 3.5, 1.5), "Shadow")   # at the top edge: vertical distance counts too
  expect_true(all(is.na(attack_zone(c(0, 0, NA), c(2.5, 2.5, 2.5), c(1.5, NA, 3.5), c(1.5, 1.5, 1.5)))))
})

traj <- function(vx0 = 0, ax = 0, vz0 = 0, az = 0) {
  tibble::tibble(plate_x = 0.5, plate_z = 2.5, vy0 = -130, ay = 28, vx0 = vx0, ax = ax, vz0 = vz0, az = az)
}

test_that("a pitch with no lateral velocity or acceleration is at its plate location at the tunnel point", {
  p <- tunnel_position(traj())
  expect_equal(p$x_tun, 0.5); expect_equal(p$z_tun, 2.5)
})

test_that("break happens late: with lateral acceleration the tunnel point is closer to where the pitch started", {
  p <- tunnel_position(traj(ax = 10))
  expect_lt(p$x_tun, 0.5)
  p2 <- tunnel_position(traj(vx0 = 5))
  expect_lt(p2$x_tun, 0.5)
  expect_gt(0.5 - p$x_tun, 0); expect_lt(abs(0.5 - p$x_tun), 1)
})

agg_display <- function() {
  set.seed(3)
  n <- 120
  tibble::tibble(
    pitch_uid = paste0("1-", seq_len(n), "-1"), pitcher = rep(c(1L, 2L), each = n / 2),
    p_throws = rep(c("R", "L"), each = n / 2), pitch_type = c(rep(c("4S", "SL"), 30), rep("SNK", 50), rep("CH", 10)),
    velo = 93, ivb = 15, hb = -8, spin_rate = 2300, ext = 6.5, arm_angle = 40,
    plate_x = rnorm(n, 0, 0.8), plate_z = rnorm(n, 2.5, 0.8), sz_top = 3.4, sz_bot = 1.6,
    x_tun = rnorm(n, 0, 0.3), z_tun = rnorm(n, 3, 0.3), stand = "R")
}

test_that("pitch profiles average each pitcher's pitch types", {
  p <- pitch_profiles(agg_display())
  expect_named(p, c("pitcher", "p_throws", "pitch_type", "n", "velo", "ivb", "hb", "spin", "ext", "arm_angle"))
  expect_equal(p$n[p$pitcher == 2 & p$pitch_type == "SNK"], 50L)
})

test_that("command shares sum to 1 per pitcher and type, with an ALL row", {
  c0 <- command_rates(agg_display())
  expect_true("ALL" %in% c0$pitch_type)
  expect_equal(c0$heart + c0$shadow + c0$chase + c0$waste, rep(1, nrow(c0)))
  expect_true(all(c0$zone >= c0$heart))
})

test_that("tunnel centroids drop pitch types under 20 pitches", {
  t0 <- tunnel_centroids(agg_display())
  expect_false(any(t0$pitcher == 2 & t0$pitch_type == "CH"))
  expect_named(t0, c("pitcher", "stand", "pitch_type", "n", "x_tun", "z_tun", "x_plate", "z_plate"))
})

test_that("profiles and command rates drop pitch types under 5 pitches", {
  d <- agg_display()
  d$pitch_type[1] <- "KN"   # a single knuckleball
  expect_false("KN" %in% pitch_profiles(d)$pitch_type)
  expect_false("KN" %in% command_rates(d)$pitch_type)
  expect_true(all(pitch_profiles(d)$n >= 5))
})

test_that("tunnel centroids are per batter side", {
  d <- agg_display(); d$stand <- rep(c("L", "R"), length.out = nrow(d))
  t0 <- tunnel_centroids(d, min_n = 10)
  expect_true("stand" %in% names(t0))
  expect_setequal(unique(t0$stand), c("L", "R"))
})
