fixes_fx <- function() { local_app_fixture(.frame = parent.frame()); load_outputs(app_config()) }

# ---- Pipeline: 90% intervals, anchors written from what was used ----
test_that("grade rows carry 90% intervals", {
  fit <- list(by_type = tibble::tibble(pitcher = 1L, pitch_type = "4S", n = 100L, est = 0, se = 0.01),
              by_pitcher = tibble::tibble(pitcher = 1L, n = 100L, est = 0, se = 0.01))
  a <- tibble::tibble(facet = "stuff", scope = c("type", "global", "pitcher"), pitch_type = c("4S", NA, NA),
                      mean = 0, unit = 0.01)
  r <- grade_rows(fit, a, "stuff")
  expect_true(all(c("grade_lo90", "grade_hi90") %in% names(r)))
  expect_false(any(c("grade_lo80", "grade_hi80") %in% names(r)))
  expect_equal(r$grade_hi90[1] - r$grade_global[1], 10 * stats::qnorm(0.95), tolerance = 1e-9)
})

test_that("the published anchors are the anchors the grades used", {
  withr::local_envvar(PV_DATA_DIR = withr::local_tempdir())
  a <- tibble::tibble(facet = "stuff", scope = "global", pitch_type = NA_character_, mean = 0.123, unit = 0.456)
  write_anchors_output(a)
  back <- readr::read_csv(store_path(ANCHORS_OUTPUT), show_col_types = FALSE)
  expect_equal(back$mean, 0.123); expect_equal(back$unit, 0.456)
})

# ---- Grades never shown outside 20-80 ----
test_that("grades and ranges are capped at 20 and 80 for display", {
  source_app()
  expect_equal(clamp_grade(c(12, 20, 55, 80, 93, NA)), c(20, 20, 55, 80, 80, NA))
  expect_equal(grade_cell(78, 70, 86), "78 (70–80)")
  expect_equal(grade_cell(22, 14, 30), "22 (20–30)")
})

test_that("loaded grade columns are capped at 20-80", {
  dir <- withr::local_tempdir(); make_app_fixture(dir)
  g <- arrow::read_parquet(file.path(dir, "outputs", "grades_MLB_2026.parquet"))
  g$grade_hi90[1] <- 95; g$grade_lo90[2] <- 5; g$grade_global[3] <- 88
  arrow::write_parquet(g, file.path(dir, "outputs", "grades_MLB_2026.parquet"))
  withr::local_envvar(PV_APP_LOCAL = "1", PV_DATA_DIR = dir)
  source_app()
  o <- load_outputs(app_config())
  cols <- c("grade_type", "grade_global", "grade_lo90", "grade_hi90")
  v <- unlist(o$grades[cols]); v <- v[!is.na(v)]
  expect_true(all(v >= 20 & v <= 80))
})

# ---- Hover text is dark ----
test_that("hover labels use dark text", {
  o <- fixes_fx()
  p <- plotly::plotly_build(movement_plot(fixture_display()[1:30, ], o$movement_ref, o$pctl, "MLB", 2026L))
  expect_equal(p$x$layout$hoverlabel$font$color, INK)
})

# ---- Pitcher dropdown A-Z by last name ----
test_that("pitchers are ordered by last name, ignoring suffixes and keeping name particles", {
  p <- tibble::tibble(level = "MLB", season = 2026L, pitcher = 1:6, teams = "DET", team_abbrev = "DET",
                      pitches = c(900L, 800L, 700L, 600L, 500L, 400L),
                      name = c("Tarik Skubal", "Logan Webb", "Lance McCullers Jr.", "Jhoan Duran",
                               "Ryan Weathers", "Elvis De La Cruz"))
  out <- pitchers_for(p, "MLB", 2026L, "All")
  expect_equal(out$name, c("Elvis De La Cruz", "Jhoan Duran", "Lance McCullers Jr.", "Tarik Skubal",
                           "Ryan Weathers", "Logan Webb"))
  expect_equal(last_name_key(c("Lance McCullers Jr.", "Ken Griffey III", NA)), c("mccullers", "griffey", NA))
})

# ---- Min pitches default to 0 everywhere ----
test_that("every min-pitches slider starts at 0", {
  suppressPackageStartupMessages(library(shiny))
  source_app()
  for (ui in list(sidebarUI("s"), leaderboardUI("l"), commandUI("c"), stuffmapUI("m"))) {
    h <- as.character(ui)
    sl <- regmatches(h, regexpr('<input[^>]*min_pitches[^>]*>', h))
    expect_length(sl, 1)
    expect_match(sl, 'data-from="0"', fixed = TRUE)
  }
})

# ---- Editor: its own lasso plot on the same page as the controls ----
editor_args <- function(o) {
  d <- fixture_display()
  list(cfg = list(local = FALSE, app_repo = "o/pitching-app", corrections_token = "t",
                  admin_hash = bcrypt::hashpw("open sesame")),
       sel = list(state = shiny::reactive(list(level = "MLB", season = 2026L, pitcher = 101L, side = "All"))),
       display = list(rows = shiny::reactive(d[d$pitcher == 101, ]),
                      edits = shiny::reactiveVal(tibble::tibble(pitch_uid = character(), new_pitch_type = character()))),
       outputs = o)
}

test_that("the editor page has its own lasso plot next to the pitch-type list", {
  o <- fixes_fx(); admin_lock_reset()
  shiny::testServer(editorServer, args = editor_args(o), {
    session$setInputs(`admin-pw` = "open sesame", `admin-login` = 1)
    h <- output$panel$html
    expect_match(h, 'id="[^"]*plot"')
    for (ty in c(PITCH_LABELS, "REMOVED")) expect_match(h, sprintf('<option value="%s"', ty))
    expect_match(output$plot, '"source":"editor"', fixed = TRUE)
    expect_match(output$plot, '"dragmode":"lasso"', fixed = TRUE)
  })
})

test_that("the movement page zooms instead of lassoing, and reports its own source", {
  o <- fixes_fx()
  b <- plotly::plotly_build(movement_plot(fixture_display()[1:30, ], o$movement_ref, o$pctl, "MLB", 2026L))
  expect_equal(b$x$layout$dragmode, "zoom")
  e <- plotly::plotly_build(movement_plot(fixture_display()[1:30, ], o$movement_ref, o$pctl, "MLB", 2026L,
                                          source = "editor", dragmode = "lasso"))
  expect_equal(e$x$source, "editor"); expect_equal(e$x$layout$dragmode, "lasso")
})

test_that("the type list is not rebuilt when the selection changes", {
  o <- fixes_fx(); admin_lock_reset()
  shiny::testServer(editorServer, args = editor_args(o), {
    session$setInputs(`admin-pw` = "open sesame", `admin-login` = 1)
    h1 <- output$panel$html
    session$setInputs(new_type = "SL", save = 1)       # status changes; the panel must not re-render
    expect_identical(output$panel$html, h1)
    expect_match(output$status$html, "Select pitches first")
  })
})

# ---- Admin lockout holds across sessions ----
test_that("five wrong passwords lock every session for 15 minutes", {
  source_app(); admin_lock_reset()
  cfg <- list(admin_hash = bcrypt::hashpw("open sesame"))
  now <- as.POSIXct("2026-11-21 12:00:00", tz = "UTC")
  withr::local_options(pv.now = now)
  shiny::testServer(adminServer, args = list(cfg = cfg), {
    for (i in 1:5) session$setInputs(pw = "nope", login = i)
  })
  shiny::testServer(adminServer, args = list(cfg = cfg), {   # a fresh session (a reload)
    session$setInputs(pw = "open sesame", login = 1)
    expect_false(is_admin())
    expect_match(output$status$html, "15 minutes")
  })
  withr::local_options(pv.now = now + 16 * 60)
  shiny::testServer(adminServer, args = list(cfg = cfg), {
    session$setInputs(pw = "open sesame", login = 1)
    expect_true(is_admin())
  })
  admin_lock_reset()
})

# ---- Leaderboard reads the sidebar scale from the start ----
test_that("the leaderboard is never built on the wrong scale at startup", {
  o <- fixes_fx()
  seen <- character()
  real <- leaderboard_table
  local_global_binding(leaderboard_table = function(...) { a <- list(...); seen <<- c(seen, a[[7]]); real(...) })
  ready <- shiny::reactiveVal(FALSE)   # the sidebar is not ready when the page first loads
  sel <- list(state = shiny::reactive({ shiny::req(ready()); list(level = "MLB", season = 2026L, team = "All",
                                                                  min_pitches = 0, scale = "type") }),
              select = function(...) NULL)
  shiny::testServer(leaderboardServer, args = list(outputs = o, sel = sel), {
    session$setInputs(level = "MLB", season = "2026", team = "All", min_pitches = 0)
    try(output$table, silent = TRUE)
    ready(TRUE); session$flushReact()
    output$table
  })
  expect_true(length(seen) > 0)
  expect_true(all(seen == "type"))
})

# ---- Stuff Map colors match the grade bins ----
test_that("stuff map colorscale steps at the grade bins with the grade colors", {
  source_app()
  cs <- grade_colorscale()
  pos <- vapply(cs, function(e) e[[1]], 0); col <- vapply(cs, function(e) e[[2]], "")
  expect_equal(range(pos), c(0, 1))
  for (g in c(25, 35, 42, 50, 57, 65, 75)) {
    i <- max(which(pos <= (g - 20) / 60))
    expect_equal(col[i], grade_color(g), info = g)
  }
})

# ---- Rolling and Command name the pitcher; Command follows the sidebar ----
test_that("rolling names the selected pitcher", {
  o <- fixes_fx()
  d <- fixture_display()
  sel <- list(state = shiny::reactive(list(level = "MLB", season = 2026L, pitcher = 101L, side = "All")))
  display <- list(rows = shiny::reactive(d[d$pitcher == 101, ]))
  shiny::testServer(rollingServer, args = list(outputs = o, sel = sel, display = display), {
    expect_match(output$head$html, "Ace One")
  })
})

test_that("command page uses the sidebar's level, season and team, and explains missing tunnels", {
  o <- fixes_fx()
  expect_false(grepl('id="c-level"', as.character(commandUI("c")), fixed = TRUE))
  sel <- list(state = shiny::reactive(list(level = "MLB", season = 2026L, team = "DET", pitcher = 999L, side = "All")))
  shiny::testServer(commandServer, args = list(outputs = o, sel = sel), {
    session$setInputs(min_pitches = 0)
    expect_gt(nrow(board()), 0)
    expect_true(all(board()$pitcher %in% o$pitchers$pitcher[grepl("DET", o$pitchers$teams)]))
    expect_match(output$tunnel_message$html, "No tunnel data")
  })
})

test_that("outputs published before the 90% switch still load (80% columns renamed until republished)", {
  dir <- withr::local_tempdir(); make_app_fixture(dir)
  f <- file.path(dir, "outputs", "grades_MLB_2026.parquet")
  g <- arrow::read_parquet(f)
  g <- dplyr::rename(g, grade_lo80 = grade_lo90, grade_hi80 = grade_hi90)
  arrow::write_parquet(g, f)
  withr::local_envvar(PV_APP_LOCAL = "1", PV_DATA_DIR = dir)
  source_app()
  o <- load_outputs(app_config())
  expect_true(all(c("grade_lo90", "grade_hi90") %in% names(o$grades)))
})
