test_that("validator flags duplicate pitch_uid", {
  expect_false(check_unique_uid(tibble::tibble(pitch_uid = c("1-1-1", "1-1-1")))$pass)
  expect_true(check_unique_uid(tibble::tibble(pitch_uid = c("1-1-1", "1-1-2")))$pass)
})

test_that("game coverage counts scheduled games that have pitches", {
  sched <- tibble::tibble(game_pk = 1:100)
  sc <- tibble::tibble(game_pk = c(1:99, 99L))
  r <- check_game_coverage(sc, sched)
  expect_equal(r$value, 0.99)
  expect_true(r$pass)
  expect_false(check_game_coverage(tibble::tibble(game_pk = 1:90), sched)$pass)
})

test_that("non-missing share check uses the 0.97 bar", {
  d <- tibble::tibble(release_speed = c(rep(1, 97), rep(NA, 3)),
                      pfx_x = c(rep(1, 96), rep(NA, 4)))
  r <- check_non_missing(d, c("release_speed", "pfx_x"))
  expect_equal(r$pass, c(TRUE, FALSE))
})

test_that("row-count bars follow the plan", {
  expect_equal(row_bar("MLB", 2020), 200000)
  expect_equal(row_bar("MLB", 2024), 650000)
  expect_equal(row_bar("AAA", 2024), 300000)
})
