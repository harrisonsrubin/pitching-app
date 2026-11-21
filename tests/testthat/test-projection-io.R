test_that("projection keys are the projections file plus one luck file per level, all public outputs", {
  k <- projection_keys(2026L)
  expect_equal(k, c("outputs/projections_2026.parquet", "outputs/luck_MLB_2026.parquet", "outputs/luck_AAA_2026.parquet"))
})

test_that("a projection run writes projections with names and both luck files", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  lines <- sim_world()
  inp <- list(lines = lines, league = league_table(lines),
              signals = tibble::tibble(level = character(), season = integer(), pitcher = integer()),
              windows = NULL, parks = NULL,
              pitchers = tibble::tibble(level = "MLB", season = 2024L, pitcher = 1L, name = "Vet Eran"))
  res <- run_projection_from(inp, 2024L, anchors = tibble::tibble(mean = 4.3, unit = 0.6), n_draws = 200L)
  keys <- write_projection_outputs(res, 2024L)
  expect_true(all(file.exists(store_path(keys))))
  p <- store_read(projections_key(2024L))
  expect_equal(p$name[p$pitcher == 1][1], "Vet Eran")
  expect_true(all(p$base_season == 2024L))
  l <- store_read(luck_key("MLB", 2024L))
  expect_true(nrow(l) > 0 && all(is.finite(l$gap)))
  expect_equal(nrow(store_read(luck_key("AAA", 2024L))), 0L)  # MLB only: AAA results don't compare to MLB-equivalent talent
})

test_that("trimming inputs drops seasons after the base season", {
  lines <- sim_world()
  t <- trim_inputs(list(lines = lines, windows = NULL), 2020L)
  expect_true(all(t$lines$season <= 2020))
  expect_null(t$windows)
})

test_that("a projection run stops when the season's grades or components are missing", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  expect_error(check_projection_inputs(2026L), "grades_MLB_2026")
  store_write(tibble::tibble(a = 1), "outputs/grades_MLB_2026.parquet")
  store_write(tibble::tibble(a = 1), "outputs/components_MLB_2026.parquet")
  store_write(tibble::tibble(a = 1), "outputs/windows_MLB_2026.parquet")
  store_write(tibble::tibble(a = 1), "outputs/parks_MLB_2026.parquet")
  store_write(tibble::tibble(a = 1), "outputs/pitchers_MLB_2026.parquet")
  expect_true(check_projection_inputs(2026L))
})

test_that("downloads use the app token and a failed download stops the run", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  calls <- list()
  local_global_binding(gh_run = function(args, token = NULL) { calls[[length(calls) + 1]] <<- list(args = args, token = token); 1L },
                       release_download_dir = function(prefix, repo) invisible(character()))
  expect_error(fetch_projection_inputs("o/data", "o/app", "app-token"), "download failed")
  expect_equal(calls[[1]]$token, "app-token")
})
