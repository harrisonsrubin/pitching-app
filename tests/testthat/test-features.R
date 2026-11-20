fx <- function() tibble::tibble(
  pitcher = 1L, season = 2030L, level = "MLB",
  pitch_type = c("4S", "4S", "SL"), pitch_group = c("FB", "FB", "BR"),
  velo = c(95, 95, 85), hb = c(8, 8, -4), ivb = c(16, 16, 2), spin_axis_m = c(210, 210, 60))

test_that("fastball differentials", {
  d <- add_fastball_diffs(fx(), primary_fastball(fx()))
  expect_equal(d$d_velo, c(0, 0, -10)); expect_equal(d$d_hb, c(0, 0, -12))
})

test_that("no fastball -> NA differentials", {
  d <- fx()[3, ]
  expect_true(is.na(add_fastball_diffs(d, primary_fastball(d))$d_velo))
})

test_that("a correction that changes the primary fastball changes the differentials", {
  d <- dplyr::bind_rows(fx(), tibble::tibble(pitcher = 1L, season = 2030L, level = "MLB",
         pitch_uid = c("9-1-1", "9-1-2", "9-1-3"), pitch_type = "SNK", pitch_group = "FB",
         velo = 92, hb = 15, ivb = 8, spin_axis_m = 230))
  before <- add_fastball_diffs(d, primary_fastball(d))           # SNK primary (3 vs 2)
  expect_equal(before$d_velo[3], -7)                              # SL vs 92 SNK
  corr <- tibble::tibble(pitch_uid = c("9-1-1", "9-1-2", "9-1-3"), new_pitch_type = "CH")
  d2 <- apply_corrections(d, corr)
  d2$pitch_group <- pitch_group(d2$pitch_type)
  after <- add_fastball_diffs(d2, primary_fastball(d2))          # now 4S primary
  expect_equal(after$d_velo[3], -10)                              # SL vs 95 4S
})

test_that("perceived velo equals velo at 6 ft extension, higher with more", {
  expect_equal(perceived_velo(95, 6), 95)
  expect_gt(perceived_velo(95, 7), 95)
})

test_that("a lefty and his mirror-image righty get identical stuff features", {
  raw <- tibble::tibble(pitcher = 1:2, p_throws = c("R", "L"), release_pos_x = c(-2, 2),
                        pfx_x = c(-0.7, 0.7), pfx_z = 1.3, spin_axis = c(210, 150), rho = RHO_REF)
  p <- prepare_mirror(raw)
  expect_equal(p$rel_x[1], p$rel_x[2]); expect_equal(p$hb[1], p$hb[2])
  expect_equal(p$spin_axis_m[1], p$spin_axis_m[2])
})

test_that("slot reference falls back to the pitch type median for thin bins", {
  d <- tibble::tibble(pitch_type = "4S", arm_angle = c(rep(42, 300), 12), hb = c(rep(8, 300), 20), ivb = 15)
  ref <- fit_slot_ref(d)
  out <- apply_slot_ref(tibble::tibble(pitch_type = "4S", arm_angle = c(43, 12), hb = c(9, 20), ivb = 15), ref)
  expect_equal(out$hb_vs_slot, c(1, 12))   # thin 10-15 bin uses the 4S median (8)
})

test_that("arm angle imputation: pitcher median first, then release-point model", {
  d <- tibble::tibble(pitcher = c(1L, 1L, 2L), season = 2030L, level = "MLB",
                      arm_angle = c(40, NA, NA), rel_z = c(6, 6, 5), rel_x = c(2, 2, 2))
  ref <- tibble::tibble(term = c("(Intercept)", "rel_z"), coef = c(-10, 5))
  out <- impute_arm_angle(d, ref)
  expect_equal(out$arm_angle, c(40, 40, 15))
  expect_equal(out$arm_angle_imputed, c(0L, 1L, 1L))
})

test_that("edge distance is negative inside and positive outside", {
  d <- add_location_features(tibble::tibble(stand = "R", p_throws = "R",
         plate_x = c(0, 1.83, 0), plate_z = c(2.5, 2.5, 5.12), sz_bot = 1.5, sz_top = 3.5))
  expect_lt(d$edge_dist[1], 0)
  expect_equal(d$edge_dist[2], 1, tolerance = 1e-9)
  expect_equal(d$edge_dist[3], 1.5, tolerance = 1e-9)
  expect_equal(d$same_side, c(1L, 1L, 1L))
})

test_that("inside is positive for both batter sides", {
  d <- add_location_features(tibble::tibble(stand = c("R", "L"), p_throws = "R",
         plate_x = c(-0.5, 0.5), plate_z = 2.5, sz_bot = 1.5, sz_top = 3.5))
  expect_equal(d$x_in, c(0.5, 0.5))
})

test_that("sequence features use the previous pitch of the same PA only", {
  d <- tibble::tibble(game_pk = 1L, at_bat_number = c(1L, 1L, 2L), pitch_number = c(1L, 2L, 1L),
                      velo = c(95, 85, 90), hb = 0, ivb = 0, x_in = 0, plate_z = 2,
                      pitch_type = c("4S", "SL", "4S"), n_thruorder_pitcher = 1L)
  s <- add_sequence_features(d)
  expect_equal(s$prev_d_velo, c(NA, -10, NA))
  expect_equal(s$prev_same_type, c(NA, 0L, NA))
})

test_that("count mix shares sum to one", {
  m <- readr::read_csv(file.path(repo_root(), "models", "count_mix.csv"), show_col_types = FALSE)
  expect_equal(sum(m$share), 1, tolerance = 1e-9)
  expect_equal(nrow(m), 24)
})
