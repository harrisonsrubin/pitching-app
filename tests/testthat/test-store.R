test_that("write/read round trip, atomic rename, listing", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  df <- tibble::tibble(a = 1:3)
  store_write(df, "x/y.parquet")
  expect_true(store_exists("x/y.parquet"))
  expect_false(store_exists("x/z.parquet"))
  expect_equal(store_read("x/y.parquet"), df)
  expect_length(list.files(dirname(store_path("x/y.parquet")), pattern = "tmp"), 0)
  expect_equal(store_list("x"), "x/y.parquet")
  expect_equal(store_list("nothing-here"), character())
})

test_that("release_upload uploads the file under its own name to the given tag", {
  calls <- list()
  local_global_binding(gh_run = function(args) { calls[[length(calls) + 1]] <<- args; 0L })
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  store_write(tibble::tibble(a = 1), "raw/a/b.parquet")
  release_upload("raw/a/b.parquet", repo = "o/r", tag = "raw")
  up <- calls[[length(calls)]]
  expect_equal(up[1:2], c("release", "upload"))
  expect_true(any(grepl("b.parquet", up, fixed = TRUE)))
  expect_true(all(c("--clobber", "o/r", "raw") %in% up))
})

test_that("release_upload errors when gh fails", {
  local_global_binding(gh_run = function(args) 1L)
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  store_write(tibble::tibble(a = 1), "k.parquet")
  expect_error(release_upload("k.parquet", repo = "o/r", tag = "raw"), "gh release")
})

test_that("each store directory gets its own release tag, created if missing", {
  calls <- list()
  local_global_binding(gh_run = function(args) {
    calls[[length(calls) + 1]] <<- args
    if (args[2] == "view") 1L else 0L
  })
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  store_write(tibble::tibble(a = 1), "raw/statcast/MLB/2026/2026-04-01.parquet")
  release_upload("raw/statcast/MLB/2026/2026-04-01.parquet", repo = "o/r")
  expect_equal(release_tag("raw/statcast/MLB/2026/2026-04-01.parquet"), "raw--statcast--MLB--2026")
  verbs <- vapply(calls, function(a) a[2], "")
  expect_equal(verbs, c("view", "create", "upload"))
  expect_true(any(grepl("2026-04-01.parquet", calls[[3]], fixed = TRUE)))
  expect_true("raw--statcast--MLB--2026" %in% calls[[3]])
})
