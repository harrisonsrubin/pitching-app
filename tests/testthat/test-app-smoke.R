test_that("app server starts on fixture data and every page renders", {
  local_app_fixture()
  app <- shiny::shinyAppDir(file.path(repo_root(), "app"))
  shiny::testServer(app, {
    session$setInputs(`sel-level` = "MLB", `sel-season` = "2026", `sel-team` = "All", `sel-pitcher` = "101",
                      `sel-side` = "All", `sel-min_pitches` = 0, `sel-scale` = "type",
                      `leaderboard-level` = "MLB", `leaderboard-season` = "2026", `leaderboard-team` = "All",
                      `leaderboard-min_pitches` = 0)
    expect_false(is.null(output$`grades-table`))
    expect_match(output$`grades-header`$html, "Ace One")
    expect_match(output$`grades-header`$html, "2027 projection")
    expect_false(is.null(output$`projection-table`))
    expect_false(is.null(output$`projections-table`))
    expect_false(is.null(output$`movement-plot`))
    expect_false(is.null(output$`location-plot`))
    expect_false(is.null(output$`leaderboard-table`))
    expect_match(output$`scorecard-body`$html, "<table")
    expect_match(output$`editor-panel`$html, "Admin password")
  })
})

test_that("manifest lists every package the app uses", {
  files <- list.files(file.path(repo_root(), "app"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  code <- unlist(lapply(files, readLines))
  used <- unique(c(unlist(regmatches(code, gregexpr("[A-Za-z][A-Za-z0-9.]*(?=::)", code, perl = TRUE))),
                   sub("library\\(([^)]+)\\)", "\\1", unlist(regmatches(code, gregexpr("library\\([^)]+\\)", code))))))
  used <- setdiff(used, c("base", "stats", "utils", "tools", "graphics", "grDevices", "methods"))
  m <- jsonlite::read_json(file.path(repo_root(), "app", "manifest.json"))
  expect_true(all(used %in% names(m$packages)), info = paste(setdiff(used, names(m$packages)), collapse = ", "))
  expect_true(file.exists(file.path(repo_root(), "app", m$metadata$entrypoint %||% "app.R")))
})

test_that("nothing pitch-level in app/", {
  f <- list.files(file.path(repo_root(), "app"), recursive = TRUE)
  expect_false(any(grepl("\\.(parquet|csv|rds)$", f)))
})

test_that("app builds in a clean R process (no pipeline globals)", {
  dir <- withr::local_tempdir(); make_app_fixture(dir)
  out <- withr::with_envvar(c(PV_APP_LOCAL = "1", PV_DATA_DIR = dir), system2(
    file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote(sprintf("a <- shiny::shinyAppDir('%s'); writeLines('APP_OK')", file.path(repo_root(), "app")))),
    stdout = TRUE, stderr = TRUE))
  expect_true(any(grepl("APP_OK", out)), info = paste(out, collapse = "\n"))
})
