lb_fx <- function() {
  local_app_fixture(.frame = parent.frame())
  o <- load_outputs(app_config())
  aaa <- o$grades[o$grades$pitcher == 101, ]; aaa$level <- "AAA"; aaa$pitcher <- 201L
  o$grades <- dplyr::bind_rows(o$grades, aaa)
  o$pitchers <- dplyr::bind_rows(o$pitchers, tibble::tibble(level = "AAA", season = 2026L, pitcher = 201L, name = "Minor Leaguer",
                                                            throws = "R", team_abbrev = "TOL", teams = "TOL", pitches = 500L))
  o
}

test_that("min_pitches filters", {
  o <- lb_fx()
  t <- leaderboard_table(o$grades, o$pitchers, "MLB", 2026L, "All", 45, "global")
  expect_setequal(t$pitcher, c(101L, 103L))
})

test_that("Both levels binds MLB and AAA", {
  o <- lb_fx()
  t <- leaderboard_table(o$grades, o$pitchers, "Both", 2026L, "All", 0, "global")
  expect_setequal(t$level, c("MLB", "AAA"))
  expect_true(201L %in% t$pitcher)
})

test_that("overall columns come from ALL rows and type columns exist", {
  o <- lb_fx()
  t <- leaderboard_table(o$grades, o$pitchers, "MLB", 2026L, "All", 0, "global")
  all101 <- o$grades[o$grades$pitcher == 101 & o$grades$level == "MLB" & o$grades$pitch_type == "ALL" & o$grades$facet == "pitching", ]
  expect_equal(t$pitching[t$pitcher == 101], all101$grade_global)
  expect_true(all(c("4S_S", "4S_L", "SL_S", "SNK_S") %in% names(t)))
  expect_equal(t$pitcher[1], t$pitcher[which.max(t$pitching)])
})

test_that("team filter keeps traded pitchers under each team", {
  o <- lb_fx()
  expect_true(102L %in% leaderboard_table(o$grades, o$pitchers, "MLB", 2026L, "DET", 0, "global")$pitcher)
})

test_that("scorecard renders the markdown and chart", {
  o <- lb_fx()
  shiny::testServer(scorecardServer, args = list(outputs = o), {
    expect_match(output$body$html, "<table")
    expect_match(output$body$html, "Stuff \\(ours\\)")
    expect_match(output$body$html, "scorecard_chart.png")
  })
})

test_that("row click selects that pitcher", {
  o <- lb_fx()
  picked <- NULL
  sel <- list(state = shiny::reactive(list(level = "MLB", season = 2026L, team = "All", min_pitches = 0, scale = "global")),
              select = function(level, season, pitcher) picked <<- list(level, season, pitcher))
  shiny::testServer(leaderboardServer, args = list(outputs = o, sel = sel, parent = NULL), {
    session$setInputs(level = "MLB", season = "2026", team = "All", min_pitches = 0)
    t <- board()
    session$setInputs(table_rows_selected = 1)
    expect_equal(picked[[3]], t$pitcher[1])
    expect_equal(picked[[1]], "MLB")
  })
})

test_that("select() moves to the pitcher even when level, season and team are unchanged", {
  local_app_fixture()
  o <- load_outputs(app_config())
  picked <- new.env(); picked$sel <- NULL
  local_global_binding(set_pitcher_choices = function(session, p, selected) picked$sel <- selected)
  shiny::testServer(sidebarServer, args = list(outputs = o), {
    session$setInputs(level = "MLB", season = "2026", team = "All", pitcher = "101")
    session$flushReact()
    expect_equal(picked$sel, 101L)
    session$returned$select("MLB", 2026L, 103L)
    session$flushReact()
    expect_equal(picked$sel, 103L)
  })
})

test_that("a season with pitchers but no grades gives an empty leaderboard, not an error", {
  o <- lb_fx()
  g <- o$grades[0, ]
  t <- leaderboard_table(g, o$pitchers, "MLB", 2026L, "All", 0, "global")
  expect_equal(nrow(t), 0)
})

test_that("the leaderboard does not rebuild when only the selected pitcher changes", {
  o <- lb_fx()
  calls <- 0L
  real <- leaderboard_table
  local_global_binding(leaderboard_table = function(...) { calls <<- calls + 1L; real(...) })
  pv <- shiny::reactiveVal(101L)
  sel <- list(state = shiny::reactive(list(level = "MLB", season = 2026L, team = "All", pitcher = pv(), scale = "global")),
              select = function(...) NULL)
  shiny::testServer(leaderboardServer, args = list(outputs = o, sel = sel, parent = NULL), {
    session$setInputs(level = "MLB", season = "2026", team = "All", min_pitches = 0)
    board(); n1 <- calls
    pv(103L); session$flushReact()
    board()
    expect_equal(calls, n1)
  })
})
