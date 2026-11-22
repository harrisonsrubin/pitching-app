proj_fx <- function() { local_app_fixture(.frame = parent.frame()); load_outputs(app_config()) }

test_that("load_outputs brings in the latest projections and the luck gaps", {
  o <- proj_fx()
  expect_true(all(c("projections", "luck") %in% names(o)))
  expect_equal(unique(o$projections$base_season), 2026L)
  expect_true(all(unlist(o$projections[paste0("chip_", 1:10)]) >= 20, na.rm = TRUE))
})

test_that("the ten chips are counted onto the 20-80 grade steps", {
  ch <- chip_counts(c(52, 54, 55, 56, 58, 61, 62, 59, 79, 81))
  expect_equal(names(ch), as.character(seq(20, 80, 5)))
  expect_equal(sum(ch), 10)
  expect_equal(unname(ch[c("50", "55", "60", "65", "80")]), c(1, 3, 4, 0, 2))
})

test_that("the Grades card lays out the bet bars, ERA, FIP, Step odds and the luck row", {
  o <- proj_fx()
  h <- as.character(projection_card(o$projections, o$luck, 101L, "MLB", 2026L))
  expect_equal(lengths(regmatches(h, gregexpr('class="pv-bar[" ]', h))), 13)
  expect_match(h, "True talent"); expect_match(h, ">62<")
  expect_match(h, "ERA"); expect_match(h, "3.20"); expect_match(h, "2.70–3.80")
  expect_match(h, "FIP"); expect_match(h, "Step forward"); expect_match(h, "12%"); expect_match(h, "6%")
  expect_match(h, "Innings"); expect_match(h, ">165<"); expect_match(h, "60–205")
  expect_match(h, "WAR"); expect_match(h, ">3.4<"); expect_match(h, "0.5 to 6.1")
  expect_match(h, "2026 luck"); expect_match(h, "BABIP"); expect_match(h, "\\+0.30")
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
  expect_true(all(c("IP", "WAR") %in% names(ot)))
  expect_equal(ot$IP[1], "165 (60–205)")
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

test_that("the Projections tab lists every projected pitcher's next-season numbers", {
  o <- proj_fx()
  t <- projections_table(o$projections, o$pitchers, basis = "All", team = "All", min_ip = 0)
  expect_setequal(t$pitcher, c(101L, 102L))
  expect_true(all(c("name", "team", "basis", "true_talent", "era", "era_lo", "era_hi", "fip", "k", "bbhbp",
                    "hr", "hbip", "step_forward", "step_back") %in% names(t)))
  expect_equal(t$era[t$pitcher == 101], 3.2)
  expect_equal(t$true_talent[t$pitcher == 101], 62)
  expect_equal(t$team[t$pitcher == 101], "DET")
  expect_true(all(c("role", "ip", "war", "war_lo", "war_hi") %in% names(t)))
  expect_equal(t$role[t$pitcher == 101], "SP"); expect_equal(t$role[t$pitcher == 102], "RP")
  expect_equal(projections_table(o$projections, o$pitchers, basis = "MLB-equivalent", team = "All", min_ip = 0)$pitcher, 102L)
  expect_equal(projections_table(o$projections, o$pitchers, basis = "All", team = "SEA", min_ip = 0)$pitcher, integer())
})

test_that("the Projections tab renders and the grade Leaderboard is back to grades only", {
  o <- proj_fx()
  sel <- list(state = shiny::reactive(list(level = "MLB", season = 2026L, pitcher = 101L, side = "All")), select = function(...) NULL)
  shiny::testServer(projectionsServer, args = list(outputs = o, sel = sel), {
    session$setInputs(basis = "All", team = "All", min_ip = 0)
    expect_false(is.null(output$table))
    expect_match(output$title$html, "2027")
  })
  t <- leaderboard_table(o$grades, o$pitchers, "MLB", 2026L, "All", 0, "global")
  expect_false(any(c("proj_era", "true_talent") %in% names(t)))
})
