test_that("outs and runs per PA in a half-inning", {
  p <- tibble::tibble(game_pk = 1L, pitcher = 10L, season = 2030L, game_date = as.Date("2030-05-01"),
         inning = 1L, inning_topbot = "Top", at_bat_number = 1:4, pitch_number = 1L,
         outs_when_up = c(0L, 0L, 0L, 1L), bat_score = c(0L, 0L, 2L, 2L), post_bat_score = c(0L, 2L, 2L, 2L),
         events = c("single", "home_run", "strikeout", "grounded_into_double_play"))
  pa <- pa_table(p)
  expect_equal(pa$outs, c(0L, 0L, 1L, 2L))   # last PA of a non-final half-inning: 3 - outs_when_up
  expect_equal(pa$runs, c(0L, 2L, 0L, 0L))
})

test_that("walk-off final half-inning uses event outs", {
  p <- tibble::tibble(game_pk = 1L, pitcher = 10L, season = 2030L, game_date = as.Date("2030-05-01"),
         inning = 9L, inning_topbot = "Bot", at_bat_number = 1:2, pitch_number = 1L,
         outs_when_up = c(0L, 1L), bat_score = c(3L, 3L), post_bat_score = c(3L, 4L),
         events = c("field_out", "home_run"))
  expect_equal(pa_table(p)$outs, c(1L, 0L))
})

test_that("park factor is regressed halfway", {
  sc <- tibble::tibble(game_pk = 1:4, season = 2030L, venue_id = c(1L, 1L, 2L, 2L),
         home_team = c("A", "A", "B", "B"), away_team = c("B", "B", "A", "A"),
         home_runs = c(6L, 6L, 3L, 3L), away_runs = c(6L, 6L, 3L, 3L))
  pf <- park_run_factors(sc)
  expect_equal(pf$pf[pf$venue_id == 1], 1 + 0.5 * (12 / 6 - 1))
})

test_that("Marcel weights 3/2/1 plus 100 IP of league", {
  h <- tibble::tibble(pitcher = 1L, season = 2027:2029, ip = c(100, 100, 100), ra9 = c(3, 4, 5), age = 26:28)   # 29 in 2030: no age factor
  m <- marcel_ra9(h, 2030, lg_ra9 = 4.5)
  expect_equal(m$marcel, (3*100*5 + 2*100*4 + 1*100*3 + 100*4.5) / (600 + 100))
})

test_that("Marcel ages: younger improves, older declines", {
  h <- tibble::tibble(pitcher = 1:2, season = 2029L, ip = 100, ra9 = 4.5, age = c(24, 34))
  m <- marcel_ra9(h, 2030, lg_ra9 = 4.5)   # ages in 2030: 25 and 35
  expect_equal(m$marcel, 4.5 * c(1 - 0.006 * 4, 1 + 0.003 * 6))
})

test_that("scores stored as text still add up", {
  p <- tibble::tibble(game_pk = 1L, season = 2030L, venue_id = 1L, home_team = "A", away_team = "B",
                      post_home_score = c("1", "3"), post_away_score = c("0", "2"))
  g <- game_scores(p)
  expect_equal(g$home_runs, 3); expect_equal(g$away_runs, 2)
})
