test_that("FanGraphs rows parse with ids, outs, and plus metrics", {
  f <- parse_fangraphs(jsonlite::read_json(test_path("fixtures", "fangraphs_synthetic.json")))
  expect_equal(f$mlbam_id, c(900001L, 900002L))
  expect_equal(f$ip_outs, c(20L, 360L))
  expect_true(all(c("era","fip","xfip","siera","k_minus_bb","stuff_plus","location_plus","pitching_plus") %in% names(f)))
  expect_true(is.na(f$stuff_plus[2]))
})

test_that("request uses the public leaderboard with all pitchers", {
  r <- fangraphs_request(2030)
  expect_match(r$url, "fangraphs.com/api/leaders/major-league/data")
  expect_match(r$url, "qual=0"); expect_match(r$url, "season=2030"); expect_match(r$url, "type=8")
})
