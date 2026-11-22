# Fixture outputs plus a 2025 MLB and 2025 AAA season for pitcher 101
career_fx <- function() {
  dir <- local_app_fixture(.frame = parent.frame())
  o <- load_outputs(app_config())
  add_season <- function(x, level, season, shift = 0) {
    r <- x[x$pitcher == 101 & x$season == 2026L, ]
    r$level <- level; r$season <- season
    if ("grade_global" %in% names(r)) r$grade_global <- r$grade_global + shift
    dplyr::bind_rows(x, r)
  }
  o$pitchers <- add_season(add_season(o$pitchers, "MLB", 2025L), "AAA", 2025L)
  o$grades <- add_season(add_season(o$grades, "MLB", 2025L, -10), "AAA", 2025L, 5)
  d <- fixture_display(); d <- d[d$pitcher == 101, ]
  d$ivb <- d$ivb - 3; d$pitch_type[d$pitch_type == "SL"] <- "CH"  # 2025: less ride, a changeup instead of a slider
  p <- file.path(dir, sprintf("display/MLB/2025/bucket_%d.parquet", 101L %% 32L))
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(d, p)
  o$pctl <- dplyr::bind_rows(o$pctl, dplyr::mutate(o$pctl, season = 2025L))
  o
}

test_that("career lines list every season and level the pitcher has, oldest first", {
  o <- career_fx()
  l <- career_lines(o$pitchers, o$grades, 101L)
  expect_equal(l$season, c(2025L, 2025L, 2026L))
  expect_equal(l$level, c("MLB", "AAA", "MLB"))
  expect_equal(l$pitching[l$level == "MLB" & l$season == 2025L], 49 - 10)
  expect_equal(nrow(career_lines(o$pitchers, o$grades, 999L)), 0)
})

test_that("season pills mark the current season and carry level and season", {
  o <- career_fx()
  html <- as.character(season_pills(career_lines(o$pitchers, o$grades, 101L), "MLB", 2026L, "grades-season_pick"))
  expect_equal(lengths(regmatches(html, gregexpr("<button", html))), 3)
  expect_match(html, "pv-pill active[^>]*MLB\\|2026")
  expect_match(html, "AAA\\|2025")
  expect_match(html, "Triple-A")
  expect_null(season_pills(career_lines(o$pitchers, o$grades, 102L), "MLB", 2026L, "x"))  # one season, no pills
})

test_that("clicking a season pill selects that season for the same pitcher", {
  o <- career_fx()
  picked <- NULL
  sel <- list(state = shiny::reactive(list(level = "MLB", season = 2026L, pitcher = 101L, side = "All", scale = "type", min_pitches = 0)),
              select = function(level, season, pitcher) picked <<- list(level, season, pitcher))
  display <- list(rows = shiny::reactive(NULL))
  shiny::testServer(gradesServer, args = list(outputs = o, sel = sel, display = display), {
    expect_match(output$header$html, "pv-pill")
    session$setInputs(season_pick = "AAA|2025")
    expect_equal(picked, list("AAA", 2025L, 101L))
  })
})

test_that("career display stacks every season that loads and skips the rest", {
  o <- career_fx()
  cache <- bucket_cache()
  d <- career_display(app_config(), "MLB", c(2024L, 2025L, 2026L), 101L, cache)
  expect_setequal(unique(d$season), c(2025L, 2026L))
  expect_true(all(d$pitcher == 101L))
  expect_null(career_display(app_config(), "MLB", 2024L, 101L, cache))
})

test_that("the movement animation has one frame per season, a play button, and averages that glide", {
  o <- career_fx()
  d <- career_display(app_config(), "MLB", c(2025L, 2026L), 101L, bucket_cache())
  b <- plotly::plotly_build(movement_animation(d, o$pctl, "MLB"))
  expect_equal(vapply(b$x$frames, function(f) f$name, ""), c("2025", "2026"))
  expect_true(any(grepl("Play", unlist(b$x$layout$updatemenus))))
  expect_length(b$x$layout$sliders, 1)
  slot <- function(nm, f) {
    k <- which(vapply(b$x$data, function(t) identical(t$name, nm), TRUE)) - 1
    b$x$frames[[f]]$data[[match(k, unlist(b$x$frames[[f]]$traces))]]
  }
  f25 <- slot("4S avg", 1); f26 <- slot("4S avg", 2)
  expect_length(slot("SL", 1)$x, 0)  # no slider in 2025
  expect_length(slot("CH", 2)$x, 0)  # no changeup in 2026
  expect_true(all(slot("CH", 1)$ids %in% d$pitch_uid[d$season == 2025L & d$pitch_type == "CH"]))
  expect_lt(f25$y, f26$y)  # 2025 had less ride
})

test_that("the movement page switches to the animation for all seasons", {
  o <- career_fx()
  cfg <- app_config()
  sel <- list(state = shiny::reactive(list(level = "MLB", season = 2026L, pitcher = 101L, side = "All")))
  d <- fixture_display()
  display <- list(rows = shiny::reactive(d[d$pitcher == 101, ]), edits = shiny::reactive(NULL))
  shiny::testServer(movementServer, args = list(outputs = o, sel = sel, display = display, cfg = cfg, cache = bucket_cache()), {
    session$setInputs(view = "all")
    expect_match(output$plot, "frames")
    session$setInputs(view = "season")
    expect_false(grepl("\"frames\":\\[\\{", output$plot))
  })
})

# 2026 fixture plus a 2025 season where the busiest and the first-by-last-name pitchers differ
sidebar_fx <- function() {
  local_app_fixture(.frame = parent.frame())
  o <- load_outputs(app_config())
  y <- o$pitchers[o$pitchers$season == 2026L, ]
  y$season <- 2025L; y$pitches <- c(100L, 900L, 50L)  # 101 One, 102 Two (busiest), 103 Pitch
  o$pitchers <- dplyr::bind_rows(o$pitchers, y)
  o
}

test_that("the pitcher list defaults to the first name by last name, not the busiest", {
  o <- sidebar_fx()
  picked <- new.env(); picked$sel <- NULL
  local_global_binding(set_pitcher_choices = function(session, p, selected) picked$sel <- selected)
  shiny::testServer(sidebarServer, args = list(outputs = o), {
    session$setInputs(level = "MLB", season = "2025", team = "All")
    session$flushReact()
    expect_equal(picked$sel, 101L)
  })
})

test_that("a season pill keeps the same pitcher even if the dropdown clears while it reloads", {
  o <- sidebar_fx()
  picked <- new.env(); picked$sel <- NULL
  local_global_binding(set_pitcher_choices = function(session, p, selected) picked$sel <- selected)
  shiny::testServer(sidebarServer, args = list(outputs = o), {
    session$setInputs(level = "MLB", season = "2026", team = "All", pitcher = "103")
    session$flushReact()
    session$returned$select("MLB", 2025L, 103L)
    session$flushReact()
    session$setInputs(pitcher = "")  # server-side selectize empties while its choices reload
    session$setInputs(season = "2025")
    session$flushReact()
    expect_equal(picked$sel, 103L)
  })
})

test_that("a jump to a season that doesn't exist leaves the sidebar working", {
  o <- sidebar_fx()
  picked <- new.env(); picked$sel <- NULL
  local_global_binding(set_pitcher_choices = function(session, p, selected) picked$sel <- selected)
  shiny::testServer(sidebarServer, args = list(outputs = o), {
    session$setInputs(level = "MLB", season = "2026", team = "All", pitcher = "103")
    session$flushReact()
    session$returned$select("MLB", 2019L, 103L)
    session$flushReact()
    picked$sel <- NULL
    session$setInputs(season = "2025")
    session$flushReact()
    expect_equal(picked$sel, 103L)
  })
})
