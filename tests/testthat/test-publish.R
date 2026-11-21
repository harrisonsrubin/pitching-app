gh_recorder <- function(status = 0L) {
  rec <- new.env(); rec$calls <- list()
  local_global_binding(gh_run = function(args, ...) { rec$calls[[length(rec$calls) + 1]] <- list(args = args, ...); status },
                       .frame = parent.frame())
  rec
}

test_that("outputs contain no pitch-level file", {
  k <- outputs_for("MLB", 2026L)
  expect_true(length(k) >= 5)
  expect_true(all(startsWith(k, "outputs/")))
  expect_false(any(grepl("display", k)))
  expect_true(all(c("grades_MLB_2026.parquet", "components_MLB_2026.parquet", "pitchers_MLB_2026.parquet",
                    "movement_ref_MLB_2026.parquet", "pitch_pctl_MLB_2026.parquet") %in% basename(k)))
})

test_that("display never goes to the public repo", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  for (k in 0:31) store_write(tibble::tibble(a = 1), display_key("MLB", 2026L, k))
  rec <- gh_recorder()
  publish_display("MLB", 2026L, data_repo = "me/pitching-data")
  args <- lapply(rec$calls, `[[`, "args")
  expect_true(all(vapply(args, function(a) "me/pitching-data" %in% a, TRUE)))
  expect_false(any(vapply(args, function(a) any(grepl("pitching-app", a)), TRUE)))
  ups <- Filter(function(a) a[2] == "upload", args)
  expect_equal(sum(vapply(ups, function(a) sum(grepl("bucket_\\d+\\.parquet$", a)), 1L)), 32)
  expect_true(all(vapply(ups, function(a) "display--MLB--2026" %in% a, TRUE)))
})

test_that("outputs go to the app repo's outputs release with the app token", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  for (k in outputs_for("MLB", 2026L)) store_write(tibble::tibble(a = 1), k)
  rec <- gh_recorder()
  publish_outputs(outputs_for("MLB", 2026L), app_repo = "me/pitching-app", token = "tok")
  up <- Filter(function(c) c$args[2] == "upload", rec$calls)
  expect_length(up, 1)
  expect_true(all(c("outputs", "me/pitching-app", "--clobber") %in% up[[1]]$args))
  expect_equal(sum(grepl("\\.(parquet|csv)$", up[[1]]$args)), length(outputs_for("MLB", 2026L)))
  expect_equal(up[[1]]$token, "tok")
})

test_that("publish stops on a gh failure", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  for (k in outputs_for("MLB", 2026L)) store_write(tibble::tibble(a = 1), k)
  gh_recorder(status = 1L)
  expect_error(publish_outputs(outputs_for("MLB", 2026L), app_repo = "me/pitching-app", token = "tok"), "gh release")
})

test_that("stage_scorecard copies the markdown and chart, pointing the image at the chart", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  k <- stage_scorecard()
  expect_setequal(k, c("outputs/scorecard.md", "outputs/scorecard_chart.png"))
  md <- readLines(store_path("outputs/scorecard.md"))
  expect_true(any(grepl("](scorecard_chart.png)", md, fixed = TRUE)))
  expect_true(file.exists(store_path("outputs/scorecard_chart.png")))
})

test_that("display publishing refuses a public or app repo", {
  local_global_binding(gh_output = function(args) "false")
  expect_error(assert_private_repo("me/pitching-data", "me/pitching-app"), "not private")
  expect_error(assert_private_repo("Me/Pitching-App", "me/pitching-app"), "app repo")
  local_global_binding(gh_output = function(args) "true")
  expect_silent(assert_private_repo("me/pitching-data", "me/pitching-app"))
  expect_match(paste(readLines(file.path(repo_root(), "run.R")), collapse = "\n"), "assert_private_repo")
})

test_that("outputs include the P2b aggregates and the grade anchors, still nothing pitch-level", {
  k <- outputs_for("MLB", 2026L)
  expect_gte(length(k), 9)   # P3a adds windows and parks (test-windows.R pins 11)
  expect_true(all(c("outputs/profiles_MLB_2026.parquet", "outputs/command_MLB_2026.parquet",
                    "outputs/tunnel_MLB_2026.parquet", "outputs/grade_anchors.csv") %in% k))
  expect_false(any(grepl("display", k)))
})
