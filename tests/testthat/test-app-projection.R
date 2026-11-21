proj_fx <- function() { local_app_fixture(.frame = parent.frame()); load_outputs(app_config()) }

test_that("load_outputs brings in the latest projections and the luck gaps", {
  o <- proj_fx()
  expect_true(all(c("projections", "luck") %in% names(o)))
  expect_equal(unique(o$projections$base_season), 2026L)
  expect_true(all(unlist(o$projections[paste0("chip_", 1:10)]) >= 20, na.rm = TRUE))
})

test_that("the Grades card shows the bet, ERA, FIP, luck and Step odds", {
  o <- proj_fx()
  h <- as.character(projection_card(o$projections, o$luck, 101L, "MLB", 2026L))
  expect_match(h, "ERA 3.2"); expect_match(h, "FIP 3.2"); expect_match(h, "Step forward 12%"); expect_match(h, "Step back 6%")
  expect_match(h, "Luck gap 2026")
  expect_equal(lengths(regmatches(h, gregexpr('class="pv-chip"', h))), 10)
  expect_false(grepl("MLB-equivalent", h))
})

test_that("no projection, no card; an older season gets no card either", {
  o <- proj_fx()
  expect_null(projection_card(o$projections, o$luck, 103L, "MLB", 2026L))
  expect_null(projection_card(o$projections, o$luck, 101L, "MLB", 2025L))
})

test_that("a mostly-Triple-A pitcher is labeled MLB-equivalent under either level filter", {
  o <- proj_fx()
  expect_match(as.character(projection_card(o$projections, o$luck, 102L, "MLB", 2026L)), "MLB-equivalent")
  expect_match(as.character(projection_card(o$projections, o$luck, 102L, "AAA", 2026L)), "MLB-equivalent")
})

test_that("the Projection tab shows the 1-5 season table and the luck breakdown", {
  o <- proj_fx()
  sel <- list(state = shiny::reactive(list(level = "MLB", season = 2026L, pitcher = 101L, side = "All")))
  ot <- outlook_table(pitcher_projection(o$projections, 101L, 2026L))
  expect_equal(ot$Season, as.character(2027:2031))
  expect_equal(ot$ERA[1], "3.20 (2.70–3.80)")
  shiny::testServer(projectionServer, args = list(outputs = o, sel = sel), {
    expect_false(is.null(output$table))
    expect_match(output$luck$html, "BABIP")
    expect_match(output$weights$html, "Pitch-model signals 40%")
  })
  sel2 <- list(state = shiny::reactive(list(level = "MLB", season = 2026L, pitcher = 103L, side = "All")))
  shiny::testServer(projectionServer, args = list(outputs = o, sel = sel2), {
    expect_match(output$message$html, "No projection")
  })
})

test_that("the leaderboard adds projected ERA and True Talent for the projection season", {
  o <- proj_fx()
  t <- leaderboard_table(o$grades, o$pitchers, "MLB", 2026L, "All", 0, "global", o$projections)
  expect_true(all(c("proj_era", "true_talent") %in% names(t)))
  expect_equal(t$true_talent[t$pitcher == 101], 62)
  expect_true(is.na(t$true_talent[t$pitcher == 103]))
})
