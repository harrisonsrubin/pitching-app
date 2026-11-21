wf_dir <- function() file.path(repo_root(), ".github", "workflows")

test_that("expected workflows exist", {
  expect_setequal(list.files(wf_dir(), pattern = "\\.ya?ml$"),
                  c("backfill.yml", "nightly.yml", "weekly.yml", "rescore.yml", "deploy.yml"))
})

run_commands <- function() {
  code <- readLines(file.path(repo_root(), "run.R"))
  sub("^cmd_([a-z_]+) <- function.*", "\\1", grep("^cmd_[a-z_]+ <- function", code, value = TRUE))
}

test_that("workflows parse and every run.R command they call exists", {
  for (f in setdiff(list.files(wf_dir(), full.names = TRUE), file.path(wf_dir(), "deploy.yml"))) {
    y <- yaml::read_yaml(f)
    trig <- y[["on"]] %||% y[["TRUE"]]
    expect_false(is.null(trig), info = f)
    expect_false(is.null(y$jobs), info = f)
    runs <- unlist(lapply(y$jobs, function(j) vapply(j$steps, function(s) s$run %||% "", "")))
    cmds <- sub("run.R ", "", unlist(regmatches(runs, gregexpr("run.R [a-z_]+", runs))), fixed = TRUE)
    expect_true(length(cmds) > 0, info = f)
    expect_true(all(cmds %in% run_commands()), info = paste(f, paste(setdiff(cmds, run_commands()), collapse = ",")))
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
  both <- grepl("run.R backfill_statcast", runs, fixed = TRUE) & grepl("run.R sync_store upload", runs, fixed = TRUE)
  expect_true(any(both))
})

test_that("weekly refreshes only the current season", {
  y <- yaml::read_yaml(file.path(wf_dir(), "weekly.yml"))
  runs <- paste(vapply(y$jobs$refresh$steps, function(s) s$run %||% "", ""), collapse = "\n")
  expect_match(runs, "run.R backfill_statsapi current", fixed = TRUE)
})

wf <- function(name) yaml::read_yaml(file.path(wf_dir(), name))
wf_trigger <- function(y) y[["on"]] %||% y[["TRUE"]]
wf_runs <- function(y) paste(unlist(lapply(y$jobs, function(j) vapply(j$steps, function(s) s$run %||% "", ""))), collapse = "\n")

test_that("changed_seasons reads only corrections files", {
  p <- c("corrections/pitch_corrections_2025.csv", "app/app.R", "corrections/pitch_corrections_2026.csv",
         "corrections/README.md", "corrections/pitch_corrections_2025.csv")
  expect_identical(changed_seasons(p), c(2025L, 2026L))
  expect_identical(changed_seasons(character()), integer())
})

test_that("levels_for 2022 is MLB only", {
  expect_equal(levels_for(2022L), "MLB")
  expect_equal(levels_for(2023L), c("MLB", "AAA"))
})

test_that("rescore runs on corrections pushes to main and queues instead of overlapping", {
  y <- wf("rescore.yml")
  t <- wf_trigger(y)
  expect_equal(t$push$branches, "main")
  expect_equal(t$push$paths, "corrections/**")
  expect_true("seasons" %in% names(t$workflow_dispatch$inputs))
  expect_false(isTRUE(y$concurrency[["cancel-in-progress"]]))
  r <- wf_runs(y)
  for (s in c("run.R changed_seasons", "run.R fetch_inputs", "run.R update_season", "run.R publish season")) expect_match(r, s, fixed = TRUE)
  expect_match(r, "--status success", fixed = TRUE)  # diff from the last successful rescore
})

test_that("deploy only fires on app changes and fast-forwards the deploy branch", {
  y <- wf("deploy.yml")
  t <- wf_trigger(y)
  expect_equal(t$push$branches, "main")
  expect_setequal(t$push$paths, c("app/**", "renv.lock"))
  expect_match(wf_runs(y), "git push origin HEAD:deploy", fixed = TRUE)
  expect_false(grepl("--force", wf_runs(y), fixed = TRUE))
  expect_equal(y$permissions$contents, "write")
})

test_that("nightly rebuilds and publishes the current season for both levels", {
  y <- wf("nightly.yml")
  r <- wf_runs(y)
  for (s in c("run.R fetch_inputs", "run.R update_season", "run.R publish season", "last_season()")) expect_match(r, s, fixed = TRUE)
  expect_equal(y$permissions$contents, "write")
})

test_that("ensure_schedule writes a live schedule for the current season when none is stored", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  local_global_binding(fetch_schedule = function(season, level) tibble::tibble(game_pk = 1L, game_date = as.Date("2027-03-26")),
                       release_download_file = function(key, repo) stop("not in store"))
  ensure_schedule("MLB", 2027L, repo = "o/d", current = TRUE)
  expect_true(store_exists(schedule_key(2027L, "MLB")))
})

test_that("nightly re-checks corrections before publishing and has time for two rebuilds", {
  y <- wf("nightly.yml")
  r <- wf_runs(y)
  expect_match(r, "git diff --quiet HEAD origin/main -- corrections/", fixed = TRUE)
  expect_lt(regexpr("git diff --quiet", r, fixed = TRUE), regexpr("run.R publish season", r, fixed = TRUE))
  expect_gte(y$jobs$ingest$`timeout-minutes`, 120)
})

test_that("a season with ingested day files is treated as current and compacted, wherever it runs", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  expect_true(is_current_season(2027L, as.Date("2027-05-01")))
  expect_false(is_current_season(2026L, as.Date("2027-05-01")))
  expect_false(has_day_files("MLB", 2027L))
  store_write(tibble::tibble(a = 1), "raw/statcast/MLB/2027/2027-03-26.parquet")
  expect_true(has_day_files("MLB", 2027L))
})

test_that("rescore takes the manual seasons input through env and validates it; last-success lookup is on main", {
  y <- wf("rescore.yml")
  r <- wf_runs(y)
  expect_false(grepl("${{ github.event.inputs.seasons }}", r, fixed = TRUE))  # never pasted into the shell
  step <- Filter(function(s) grepl("run.R changed_seasons", s$run %||% ""), y$jobs$rescore$steps)[[1]]
  expect_equal(step$env$SEASONS_INPUT, "${{ github.event.inputs.seasons }}")
  expect_match(r, "=~ ^[0-9]{4}(\\ [0-9]{4})*$", fixed = TRUE)  # whole-string bash match, not grep
  expect_match(r, "--branch main", fixed = TRUE)
})

test_that("deploy runs queue instead of racing", {
  y <- wf("deploy.yml")
  expect_equal(y$concurrency$group, "deploy")
  expect_false(isTRUE(y$concurrency[["cancel-in-progress"]]))
})

test_that("the rescore seasons check rejects multi-line and malformed input", {
  y <- wf("rescore.yml")
  run <- Filter(function(s) grepl("run.R changed_seasons", s$run %||% ""), y$jobs$rescore$steps)[[1]]$run
  lines <- strsplit(run, "\n")[[1]]
  start <- grep("SEASONS_INPUT", lines)[2]  # the validation block after the -n test
  check <- paste(lines[start:(start + 2)], collapse = "\n")
  ok <- function(input) {
    f <- tempfile(fileext = ".sh")
    writeLines(c("#!/bin/bash", check, "echo PASSED"), f)
    out <- suppressWarnings(system2("bash", f, stdout = TRUE, stderr = TRUE, env = paste0("SEASONS_INPUT=", shQuote(input))))
    any(out == "PASSED")
  }
  expect_true(ok("2025 2026"))
  expect_true(ok("2026"))
  expect_false(ok("2025\nsystem('id')"))
  expect_false(ok("2025;id"))
  expect_false(ok("25"))
})

test_that("every workflow installs the R version renv.lock was built with", {
  lock_r <- jsonlite::read_json(file.path(repo_root(), "renv.lock"))$R$Version
  for (f in setdiff(list.files(wf_dir(), full.names = TRUE), file.path(wf_dir(), "deploy.yml"))) {
    steps <- unlist(lapply(yaml::read_yaml(f)$jobs, function(j) j$steps), recursive = FALSE)
    setup <- Filter(function(s) grepl("r-lib/actions/setup-r@", s$uses %||% ""), steps)
    expect_length(setup, 1)
    expect_equal(as.character(setup[[1]]$with$`r-version`), lock_r, info = f)
  }
})

test_that("nightly, weekly and rescore run the projections, with the app repo token", {
  for (f in c("nightly.yml", "weekly.yml", "rescore.yml")) {
    y <- yaml::read_yaml(file.path(wf_dir(), f))
    runs <- unlist(lapply(y$jobs, function(j) vapply(j$steps, function(s) s$run %||% "", "")))
    expect_true(any(grepl("run.R project", runs, fixed = TRUE)), info = f)
    env <- unlist(lapply(y$jobs, function(j) names(j$env)))
    expect_true(all(c("PV_APP_REPO", "APP_GH_TOKEN", "PV_DATA_REPO") %in% env), info = f)
  }
})

test_that("run.R lists its commands and runs one in a clean process", {
  withr::local_dir(repo_root())
  out <- system2(file.path(R.home("bin"), "Rscript"), "run.R", stdout = TRUE, stderr = TRUE)
  expect_true(any(grepl("Commands:.*project", out)), info = paste(out, collapse = "\n"))
  out2 <- system2(file.path(R.home("bin"), "Rscript"), c("run.R", "changed_seasons", "HEAD", "HEAD"), stdout = TRUE, stderr = TRUE)
  expect_false(any(grepl("Error", out2)), info = paste(out2, collapse = "\n"))
})
