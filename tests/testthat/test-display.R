disp_fixture <- function(n_pitchers = 40, per = 30) {
  set.seed(1)
  n <- n_pitchers * per
  f <- tibble::tibble(
    pitch_uid = paste0("1-", seq_len(n), "-1"), game_pk = 1L, game_date = as.Date("2026-05-01"),
    pitcher = rep(seq_len(n_pitchers) * 7L, each = per), batter = 9L, stand = "R",
    p_throws = rep(c("R", "L"), length.out = n_pitchers)[rep(seq_len(n_pitchers), each = per)],
    pitch_type = sample(c("4S", "SL"), n, TRUE), balls = 0L, strikes = 0L,
    description = "ball", events = NA_character_, velo = 94, spin_rate = 2300,
    ivb = 16, hb = 10, ext = 6.5, rel_x = 2, rel_z = 5.8, arm_angle = 40, arm_angle_imputed = 0L, vaa = -5,
    plate_x = 0, plate_z = 2.5, sz_top = 3.4, sz_bot = 1.6, target = 0, extra_col = 1)
  x <- tibble::tibble(pitch_uid = f$pitch_uid, stuff_xrv = rnorm(n, 0, 0.02),
                      location_xrv = rnorm(n, 0, 0.02), pitching_xrv = rnorm(n, 0, 0.02))
  sv <- tibble::tibble(pitch_uid = f$pitch_uid, pitch_type_savant = f$pitch_type)
  list(f = f, x = x, sv = sv)
}

test_that("display has exactly the listed columns", {
  z <- disp_fixture()
  d <- display_rows(z$f, z$x, z$sv)
  expect_named(d, DISPLAY_COLS)
})

test_that("lefty hb and rel_x are unmirrored", {
  z <- disp_fixture()
  d <- display_rows(z$f, z$x, z$sv)
  expect_true(all(d$hb[d$p_throws == "L"] == -10)); expect_true(all(d$hb[d$p_throws == "R"] == 10))
  expect_true(all(d$rel_x[d$p_throws == "L"] == -2))
})

test_that("every pitch lands in exactly one bucket and buckets cover the season", {
  z <- disp_fixture()
  d <- display_rows(z$f, z$x, z$sv)
  b <- split_buckets(d)
  expect_length(b, N_BUCKETS)
  expect_equal(sum(vapply(b, nrow, 1L)), nrow(d))
  owners <- unlist(lapply(seq_along(b), function(i) unique(b[[i]]$pitcher)))
  expect_false(any(duplicated(owners)))
  expect_true(all(b[[pitcher_bucket(7L) + 1L]]$pitcher %% N_BUCKETS == 7L %% N_BUCKETS))
})

test_that("empty bucket keeps the schema", {
  z <- disp_fixture(n_pitchers = 2)
  b <- split_buckets(display_rows(z$f, z$x, z$sv))
  empty <- b[[which(vapply(b, nrow, 1L) == 0)[1]]]
  expect_named(empty, DISPLAY_COLS)
})

test_that("display_key names the bucket file", {
  expect_equal(display_key("MLB", 2026, 3L), "display/MLB/2026/bucket_3.parquet")
  expect_equal(release_tag(display_key("MLB", 2026, 3L)), "display--MLB--2026")
})

test_that("pctl_of: lowest xRV is the 100th percentile", {
  q <- stats::quantile(seq(-1, 1, length.out = 1001), seq(0, 1, 0.01), names = FALSE)
  expect_equal(pctl_of(c(-5, 5, 0), q), c(100, 0, 50))
})

test_that("pitch_percentiles has 101 quantiles per type and facet", {
  z <- disp_fixture()
  p <- pitch_percentiles(display_rows(z$f, z$x, z$sv))
  expect_setequal(paste(p$pitch_type, p$facet),
                  paste(rep(c("4S", "SL"), each = 3), c("stuff", "location", "pitching")))
  expect_true(all(vapply(p$q, length, 1L) == 101L))
})

test_that("movement_ref drops cells under 200 pitches", {
  z <- disp_fixture(n_pitchers = 40, per = 100)
  r <- movement_ref(display_rows(z$f, z$x, z$sv))
  expect_true(all(r$n >= 200))
  expect_named(r, c("p_throws", "pitch_type", "arm_decile", "arm_lo", "arm_hi", "n", "mean_hb", "mean_ivb", "sd_hb", "sd_ivb", "rho"))
})
