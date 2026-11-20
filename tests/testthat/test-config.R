test_that("cfg has the spec's seasons and sport ids", {
  c <- cfg()
  expect_equal(c$seasons_mlb, 2020:2026)
  expect_equal(c$seasons_aaa, 2023:2026)
  expect_equal(c$seasons_stats, 2010:2026)
  expect_equal(c$sport_ids, c(MLB = 1L, AAA = 11L))
  expect_false(grepl("@", c$user_agent))
})

test_that("PV_DATA_DIR overrides data_dir", {
  withr::with_envvar(c(PV_DATA_DIR = "/tmp/pvx"), expect_equal(cfg()$data_dir, "/tmp/pvx"))
})

test_that("data_dir defaults to <repo>/data", {
  withr::with_envvar(c(PV_DATA_DIR = NA), expect_equal(cfg()$data_dir, file.path(repo_root(), "data")))
})

test_that("the loader defines %>% itself, so scripts work without testthat", {
  expect_true(exists("%>%", envir = globalenv(), inherits = FALSE))
})

test_that("P1 config values", {
  c0 <- cfg()
  expect_equal(c0$seasons_train, 2023:2026)
  expect_equal(c0$seasons_ref, 2023:2025)
  expect_equal(c0$n_folds, 5L)
  expect_equal(c0$lgb_params$num_threads, 6L)
  expect_true(requireNamespace("lightgbm", quietly = TRUE))
  expect_true(requireNamespace("lme4", quietly = TRUE))
})
