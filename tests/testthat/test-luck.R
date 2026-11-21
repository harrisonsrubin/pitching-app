luck_fx <- function() {
  line <- tibble::tibble(pitcher = 1:3, season = 2025L, level = "MLB", tbf = 700, k = 180, bbhbp = 60, hr = c(20, 30, 20),
                         bip = c(440, 430, 440), hbip = c(130, 130, 150), outs = 540, r = c(70, 90, 85), er = c(64, 83, 78),
                         h = c(150, 160, 170), bb = 55, hbp = 5)
  expected <- tibble::tibble(pitcher = 1:3, k = 180 / 700, bbhbp = 60 / 700, hr = 20 / 700, hbip = 130 / 440)
  map <- stats::lm(ra9_adj ~ k_r + bbhbp_r + hr_r + babip,
                   data = data.frame(ra9_adj = c(4, 5, 3, 4.5, 3.8), k_r = c(.2, .18, .3, .22, .25), bbhbp_r = c(.08, .1, .06, .09, .07),
                                     hr_r = c(.03, .035, .02, .028, .03), babip = c(.29, .31, .28, .3, .29)))
  strand <- stats::lm(lob ~ k_rate + bbhbp_rate, data = data.frame(lob = c(.72, .7, .75, .71), k_rate = c(.2, .18, .28, .22),
                                                               bbhbp_rate = c(.08, .1, .07, .09)))
  list(line = line, expected = expected, map = map, strand = strand, fb = tibble::tibble(pitcher = 1:2, fb = c(150, 160)))
}

test_that("strand rate follows the LOB% formula and is NA without base runners", {
  expect_equal(lob_pct(150, 55, 5, 70, 20), (150 + 55 + 5 - 70) / (150 + 55 + 5 - 1.4 * 20))
  expect_true(is.na(lob_pct(0, 0, 0, 0, 0)))
})

test_that("luck pieces add up to the gap, in ERA", {
  f <- luck_fx()
  g <- luck_gap(f$line, f$expected, f$map, er_r = 0.92, strand = f$strand, fb = f$fb)
  expect_equal(g$babip + g$strand + dplyr::coalesce(g$hr_fb, 0) + g$other, g$gap, tolerance = 1e-9)
  expect_equal(g$gap, g$actual_era - g$expected_era)
  expect_equal(g$actual_era[1], 27 * 64 / 540)
})

test_that("a pitcher whose balls in play fell as expected has no BABIP luck; extra homers show as HR/FB", {
  f <- luck_fx()
  g <- luck_gap(f$line, f$expected, f$map, er_r = 0.92, strand = f$strand, fb = f$fb)
  expect_equal(g$babip[g$pitcher == 1], 0)
  expect_equal(g$hr_fb[g$pitcher == 1], 0)
  expect_gt(g$hr_fb[g$pitcher == 2], 0) # more homers than his talent: unlucky, ERA up
})

test_that("without fly-ball data HR/FB is NA and other absorbs it", {
  f <- luck_fx()
  g <- luck_gap(f$line, f$expected, f$map, er_r = 0.92, strand = f$strand, fb = f$fb)
  expect_true(is.na(g$hr_fb[g$pitcher == 3]))
  expect_true(all(is.finite(g$other)))
})

test_that("pitchers under 20 innings get no luck gap", {
  f <- luck_fx()
  f$line$outs[3] <- 30
  g <- luck_gap(f$line, f$expected, f$map, er_r = 0.92, strand = f$strand, fb = f$fb)
  expect_false(3L %in% g$pitcher)
})

test_that("expected results are put in the pitcher's parks, so a hitters' park is not 'bad luck'", {
  f <- luck_fx()
  parks <- tibble::tibble(pitcher = 1:3, ppf_runs = 1.2, ppf_hr = 1.25, ppf_hbip = 1.1)
  line <- f$line
  line$hbip <- round(f$expected$hbip * 1.1 * line$bip)   # results exactly as his talent in these parks
  g0 <- luck_gap(line, f$expected, f$map, er_r = 0.92, strand = f$strand, fb = f$fb)
  g1 <- luck_gap(line, f$expected, f$map, er_r = 0.92, strand = f$strand, fb = f$fb, parks = parks)
  expect_lt(abs(g1$babip[1]), 0.01)
  expect_gt(abs(g0$babip[1]), 0.01)
  expect_equal(g1$expected_era[1], g0$expected_era[1] * 1.2)
})
