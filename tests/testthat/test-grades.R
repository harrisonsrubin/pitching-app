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

grade_season_fixture <- function() {
  s <- sim()
  s$location_xrv <- s$stuff_xrv; s$pitching_xrv <- s$stuff_xrv
  s$game_pk <- rep_len(1:40, nrow(s)); s$game_date <- as.Date("2026-04-01") + s$game_pk
  s$p_swing <- 0.5; s$p_whiff <- 0.2; s$p_cstrike <- 0.3; s$xwoba_contact <- 0.35; s$p_gb <- 0.4
  s$edge_dist <- rep(c(-0.2, 0.2), length.out = nrow(s))
  s
}

test_that("grade_season matches grade_rows for the full season, with halves for MLB only", {
  x <- grade_season_fixture()
  fits <- lapply(c("stuff", "location", "pitching"), function(f) list(fit_grades(x, paste0(f, "_xrv"))))
  anchors <- dplyr::bind_rows(Map(grade_anchors, fits, c("stuff", "location", "pitching")))
  g <- grade_season(x, "MLB", 2026L, anchors)
  expect_setequal(unique(g$grades$half), c("full", "odd", "even"))
  full <- g$grades[g$grades$half == "full" & g$grades$facet == "stuff", ]
  ref <- grade_rows(fit_grades(x, "stuff_xrv"), anchors, "stuff")
  expect_equal(full$grade_global, ref$grade_global)
  expect_equal(unique(g$grades$level), "MLB"); expect_equal(unique(g$grades$season), 2026L)
  expect_true(all(c("ALL", "4S", "SL") %in% g$components$pitch_type))
  a <- grade_season(x, "AAA", 2026L, anchors)
  expect_equal(unique(a$grades$half), "full")
})

test_that("read_anchors reads the saved anchor file", {
  a <- read_anchors()
  expect_named(a, c("facet", "scope", "pitch_type", "mean", "unit"))
  expect_setequal(unique(a$facet), c("stuff", "location", "pitching"))
})
