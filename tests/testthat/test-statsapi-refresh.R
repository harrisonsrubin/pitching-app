test_that("past seasons are skipped when cached; the current season is always refetched", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  store_write(tibble::tibble(a = 1), "raw/statsapi/pitching/MLB/2025.parquet")
  store_write(tibble::tibble(a = 1), "raw/statsapi/pitching/MLB/2026.parquet")
  expect_false(needs_fetch("raw/statsapi/pitching/MLB/2025.parquet", 2025, current = 2026))
  expect_true(needs_fetch("raw/statsapi/pitching/MLB/2026.parquet", 2026, current = 2026))
  expect_true(needs_fetch("raw/statsapi/pitching/MLB/2024.parquet", 2024, current = 2026))
})
