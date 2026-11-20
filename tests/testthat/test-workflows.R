wf_dir <- function() file.path(repo_root(), ".github", "workflows")

test_that("expected workflows exist", {
  expect_setequal(list.files(wf_dir(), pattern = "\\.ya?ml$"),
                  c("backfill.yml", "nightly.yml", "weekly.yml"))
})

test_that("workflows parse and every referenced script exists", {
  for (f in list.files(wf_dir(), full.names = TRUE)) {
    y <- yaml::read_yaml(f)
    trig <- y[["on"]] %||% y[["TRUE"]]
    expect_false(is.null(trig), info = f)
    expect_false(is.null(y$jobs), info = f)
    runs <- unlist(lapply(y$jobs, function(j) vapply(j$steps, function(s) s$run %||% "", "")))
    scripts <- unlist(regmatches(runs, gregexpr("scripts/[a-z_]+\\.R", runs)))
    expect_true(length(scripts) > 0, info = f)
    expect_true(all(file.exists(file.path(repo_root(), scripts))), info = f)
    expect_true(any(grepl("tests/testthat.R", runs, fixed = TRUE)), info = f)
  }
})

test_that("backfill is manual-only with level/start/end inputs", {
  y <- yaml::read_yaml(file.path(wf_dir(), "backfill.yml"))
  trig <- y[["on"]] %||% y[["TRUE"]]
  expect_equal(names(trig), "workflow_dispatch")
  expect_setequal(names(trig$workflow_dispatch$inputs), c("level", "start", "end"))
})

test_that("backfill uploads each season inside the same loop that fetches it", {
  y <- yaml::read_yaml(file.path(wf_dir(), "backfill.yml"))
  runs <- vapply(y$jobs$backfill$steps, function(s) s$run %||% "", "")
  both <- grepl("backfill_statcast.R", runs, fixed = TRUE) & grepl("sync_store.R upload", runs, fixed = TRUE)
  expect_true(any(both))
})

test_that("weekly refreshes only the current season", {
  y <- yaml::read_yaml(file.path(wf_dir(), "weekly.yml"))
  runs <- paste(vapply(y$jobs$refresh$steps, function(s) s$run %||% "", ""), collapse = "\n")
  expect_match(runs, "backfill_statsapi.R current", fixed = TRUE)
})
