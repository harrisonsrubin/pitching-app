stat_row <- function(player_id, season = 2025L, level = "MLB", team_id = 1L, age = 27, g = 30L, gs = 30L,
                     ip_outs = 540L, tbf = 750, so = 200, bb = 50, hbp = 8, hr = 20, h = 150, r = 70, er = 64) {
  tibble::tibble(player_id = as.integer(player_id), season = season, level = level, team_id = team_id, age = age,
                 g = g, gs = gs, ip_outs = ip_outs, tbf = tbf, so = so, bb = bb, hbp = hbp, hr = hr, h = h,
                 r = r, er = er, sv = 0, w = 0, l = 0)
}

test_that("season lines sum a traded pitcher's teams and derive balls in play", {
  s <- dplyr::bind_rows(stat_row(1, team_id = 1L, tbf = 400, so = 100, bb = 30, hbp = 5, hr = 10, h = 80, gs = 10L, g = 20L),
                        stat_row(1, team_id = 2L, tbf = 300, so = 80, bb = 20, hbp = 3, hr = 8, h = 70, gs = 0L, g = 15L),
                        stat_row(2))
  l <- season_lines(s)
  one <- l[l$pitcher == 1, ]
  expect_equal(nrow(one), 1)
  expect_equal(one$tbf, 700); expect_equal(one$k, 180); expect_equal(one$bbhbp, 58); expect_equal(one$hr, 18)
  expect_equal(one$bip, 700 - 180 - 50 - 8 - 18)
  expect_equal(one$hbip, 150 - 18)
  expect_equal(one$gs_share, 10 / 35)
})

test_that("a missing age is filled with the league-season median and flagged", {
  s <- dplyr::bind_rows(stat_row(1, age = 25), stat_row(2, age = 29), stat_row(3, age = 31), stat_row(4, age = NA))
  l <- season_lines(s)
  expect_equal(l$age[l$pitcher == 4], 29)
  expect_true(l$age_missing[l$pitcher == 4])
  expect_false(any(l$age_missing[l$pitcher != 4]))
})

test_that("league table pools rates, ER/R and the FIP constant", {
  s <- dplyr::bind_rows(stat_row(1, r = 50, er = 46), stat_row(2, r = 50, er = 46))
  lg <- league_table(season_lines(s))
  expect_equal(lg$er_r, 0.92)
  expect_equal(lg$lg_k, 400 / 1500)
  ip <- 2 * 540 / 3
  lg_era <- 9 * 92 / ip
  expect_equal(lg$fip_const, lg_era - (13 * 40 + 3 * 116 - 2 * 400) / ip)
  expect_equal(lg$lg_ra9, 27 * 100 / (2 * 540))
})

test_that("pitch signals take grades from ALL rows and usage-weight the expected rates", {
  grades <- tibble::tibble(level = "MLB", season = 2025L, pitcher = 1L, half = "full",
                           facet = c("stuff", "location", "pitching", "stuff"), pitch_type = c("ALL", "ALL", "ALL", "4S"),
                           grade_global = c(60, 45, 55, 70))
  comps <- tibble::tibble(level = "MLB", season = 2025L, pitcher = 1L, pitch_type = c("4S", "SL"), n = c(100, 300),
                          x_swing = 0.5, x_whiff_per_swing = c(0.3, 0.1), x_chase = 0.3, x_cstrike_per_take = 0.2,
                          x_woba_contact = 0.35, x_gb = c(0.4, 0.5))
  p <- pitch_signals(grades, comps)
  expect_equal(nrow(p), 1)
  expect_equal(p$stuff, 60); expect_equal(p$location, 45); expect_equal(p$pitching, 55)
  expect_equal(p$x_whiff, 0.15)
  expect_equal(p$x_gb, 0.475)
})
