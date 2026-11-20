sim <- function() {
  set.seed(4)
  p <- tibble::tibble(pitcher = 1:200, u = rnorm(200, 0, 0.004), n = c(3L, rep(400L, 199)))
  tidyr::uncount(p, n, .remove = FALSE) %>%
    dplyr::mutate(pitch_type = sample(c("4S", "SL"), dplyr::n(), TRUE),
                  stuff_xrv = ifelse(pitch_type == "4S", 0.002, -0.004) + u + rnorm(dplyr::n(), 0, 0.02))
}

test_that("small samples shrink toward the type mean with wide intervals; nothing is NA", {
  f <- fit_grades(sim(), "stuff_xrv")
  small <- f$by_pitcher[f$by_pitcher$pitcher == 1, ]
  big_se <- median(f$by_pitcher$se[f$by_pitcher$pitcher != 1])
  expect_gt(small$se, 3 * big_se)
  expect_false(any(is.na(f$by_type$est)) || any(is.na(f$by_type$se)))
})

test_that("grade direction and interval", {
  expect_equal(to_grade(-0.01, 0, 0.01), 60)
  expect_equal(to_grade(0.01, 0, 0.01), 40)
})

test_that("estimates track true talent", {
  s <- sim(); f <- fit_grades(s, "stuff_xrv")
  truth <- dplyr::distinct(s, pitcher, u)
  j <- dplyr::inner_join(f$by_pitcher, truth, by = "pitcher")
  expect_gt(cor(j$est, j$u), 0.5)
})

test_that("expected whiff rate is per swing", {
  x <- tibble::tibble(pitcher = 1L, pitch_type = "SL", p_swing = c(1, 0), p_whiff = c(0.4, 0.9),
                      p_cstrike = 0.5, xwoba_contact = 0.3, p_gb = 0.5, edge_dist = c(0.2, -0.2))
  r <- component_rates(x)
  expect_equal(r$x_whiff_per_swing[r$pitch_type == "SL"], 0.4)
  expect_equal(r$x_chase[r$pitch_type == "SL"], 1)
})
