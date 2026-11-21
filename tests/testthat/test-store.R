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

test_that("release_download_file asks gh for one asset of the key's release", {
  calls <- list()
  local_global_binding(gh_run = function(args) { calls[[length(calls) + 1]] <<- args; 0L })
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  release_download_file("raw/statcast/MLB/2026.parquet", repo = "o/p")
  a <- calls[[1]]
  expect_equal(a[1:3], c("release", "download", "raw--statcast--MLB"))
  expect_true(all(c("--pattern", "2026.parquet", "--clobber", "o/p") %in% a))
  expect_true(dir.exists(store_path("raw/statcast/MLB")))
})

test_that("release_download_file errors when gh fails", {
  local_global_binding(gh_run = function(args) 1L)
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  expect_error(release_download_file("raw/x/a.parquet", repo = "o/p"), "gh release download")
})

test_that("gh_run passes an argument with spaces as one argument", {
  bin <- withr::local_tempdir()
  out <- file.path(bin, "args.txt")
  writeLines(c("#!/bin/sh", sprintf("for a in \"$@\"; do echo \"$a\" >> '%s'; done", out)), file.path(bin, "gh"))
  Sys.chmod(file.path(bin, "gh"), "755")
  withr::local_envvar(PATH = paste(bin, Sys.getenv("PATH"), sep = ":"))
  gh_run(c("release", "create", "t", "--notes", "Data store"))
  expect_equal(readLines(out), c("release", "create", "t", "--notes", "Data store"))
  unlink(out)
  gh_output(c("api", "x", "--jq", ".private == true"))
  expect_equal(readLines(out), c("api", "x", "--jq", ".private == true"))
})

test_that("gh_run never opens a pager", {
  bin <- withr::local_tempdir()
  out <- file.path(bin, "pager.txt")
  writeLines(c("#!/bin/sh", sprintf("echo \"$GH_PAGER|$PAGER\" > '%s'", out)), file.path(bin, "gh"))
  Sys.chmod(file.path(bin, "gh"), "755")
  withr::local_envvar(PATH = paste(bin, Sys.getenv("PATH"), sep = ":"), GH_PAGER = "less", PAGER = "less")
  gh_run(c("release", "view", "t"))
  expect_equal(readLines(out), "cat|cat")
  gh_run(c("release", "view", "t"), token = "x")
  expect_equal(readLines(out), "cat|cat")
})

test_that("release_upload_dir skips files already uploaded and batches the rest per release", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  for (d in c("2026-04-01", "2026-04-02", "2026-04-03")) store_write(tibble::tibble(a = 1:3), sprintf("raw/x/2026/%s.parquet", d))
  store_write(tibble::tibble(a = 1), "raw/x/2025.parquet")
  same <- file.size(store_path("raw/x/2026/2026-04-01.parquet"))
  calls <- list()
  local_global_binding(
    gh_run = function(args, ...) { calls[[length(calls) + 1]] <<- args; 0L },
    gh_output = function(args) if ("raw--x--2026" %in% args)
      c(sprintf("2026-04-01.parquet\t%d", same), "2026-04-02.parquet\t1") else character())
  release_upload_dir("raw/x", repo = "o/d", batch = 50)
  ups <- Filter(function(a) a[2] == "upload", calls)
  expect_length(ups, 2)  # one per release
  up26 <- Filter(function(a) "raw--x--2026" %in% a, ups)[[1]]
  expect_false(any(grepl("2026-04-01.parquet", up26)))  # same size: skipped
  expect_true(any(grepl("2026-04-02.parquet", up26)))  # size differs: re-sent
  expect_true(any(grepl("2026-04-03.parquet", up26)))  # missing: sent
  expect_true("--clobber" %in% up26)
})

test_that("release_upload_dir with force re-sends everything, in batches", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  for (k in 0:4) store_write(tibble::tibble(a = k), sprintf("display/MLB/2026/bucket_%d.parquet", k))
  calls <- list()
  local_global_binding(gh_run = function(args, ...) { calls[[length(calls) + 1]] <<- args; 0L },
                       gh_output = function(args) stop("no listing when forced"))
  release_upload_dir("display/MLB/2026", repo = "o/d", batch = 2, force = TRUE)
  ups <- Filter(function(a) a[2] == "upload", calls)
  expect_length(ups, 3)
  expect_equal(sum(vapply(ups, function(a) sum(grepl("bucket_", a)), 1L)), 5)
})

test_that("release_upload_dir stops when gh fails", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  store_write(tibble::tibble(a = 1), "raw/y/a.parquet")
  local_global_binding(gh_run = function(args, ...) if (args[2] == "upload") 1L else 0L, gh_output = function(args) character())
  expect_error(release_upload_dir("raw/y", repo = "o/d"), "gh release upload")
})

test_that("release_upload_dir pauses after each batch to stay under GitHub's upload limit", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  for (k in 0:4) store_write(tibble::tibble(a = k), sprintf("display/MLB/2026/bucket_%d.parquet", k))
  waits <- numeric()
  local_global_binding(gh_run = function(args, ...) 0L, gh_output = function(args) character(),
                       upload_pause = function(seconds) waits <<- c(waits, seconds))
  release_upload_dir("display/MLB/2026", repo = "o/d", batch = 2, force = TRUE, seconds_per_file = 1.5)
  expect_equal(waits, c(3, 3, 1.5))
})

test_that("store listing includes the pitch-model booster files, so they get synced", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  dir.create(store_path("models/lgb/stuff"), recursive = TRUE)
  writeLines("tree", store_path("models/lgb/stuff/fold1.txt"))
  writeLines("junk", store_path("models/lgb/stuff/.DS_Store"))
  expect_equal(store_list("models/lgb"), "models/lgb/stuff/fold1.txt")
})
