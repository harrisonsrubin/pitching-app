fx <- function() { local_app_fixture(.frame = parent.frame()); load_outputs(app_config()) }

test_that("traded pitcher appears under both teams", {
  o <- fx()
  expect_true(102L %in% pitchers_for(o$pitchers, "MLB", 2026L, "DET")$pitcher)
  expect_true(102L %in% pitchers_for(o$pitchers, "MLB", 2026L, "NYY")$pitcher)
  expect_false(101L %in% pitchers_for(o$pitchers, "MLB", 2026L, "NYY")$pitcher)
  expect_setequal(team_choices(o$pitchers, "MLB", 2026L), c("DET", "NYY", "SEA"))
  expect_equal(pitchers_for(o$pitchers, "MLB", 2026L, "All")$pitcher, c(101L, 103L, 102L))   # by pitches
})

test_that("grades_table has one row per type plus ALL, sorted by usage", {
  o <- fx()
  d <- fixture_display(); d <- d[d$pitcher == 103, ]
  d$pitch_type[1:30] <- "CB"   # CB most used
  t <- grades_table(o$grades, o$components, 103L, "MLB", 2026L, pitch_summary(d, "All"), "global")
  expect_equal(t$pitch_type, c("CB", "4S", "CH", "ALL"))
  expect_equal(t$usage[1], mean(d$pitch_type == "CB"))
})

test_that("type scale shifts the range with the grade", {
  o <- fx()
  t <- grades_table(o$grades, o$components, 101L, "MLB", 2026L, NULL, "type")
  r <- o$grades[o$grades$pitcher == 101 & o$grades$pitch_type == "4S" & o$grades$facet == "stuff", ]
  shift <- r$grade_type - r$grade_global
  expect_equal(t$Stuff[t$pitch_type == "4S"],
               sprintf("%d (%d–%d)", round(r$grade_type), round(r$grade_lo80 + shift), round(r$grade_hi80 + shift)))
  expect_equal(t$stuff_value[t$pitch_type == "4S"], r$grade_type)
  g <- grades_table(o$grades, o$components, 101L, "MLB", 2026L, NULL, "global")
  expect_equal(g$stuff_value[g$pitch_type == "4S"], r$grade_global)
  expect_false(is.na(t$stuff_value[t$pitch_type == "ALL"]))   # overall row always uses the pitcher scale
})

test_that("NA grades and NaN components render as blank, not NaN", {
  o <- fx()
  t <- grades_table(o$grades, o$components, 103L, "MLB", 2026L, NULL, "global")
  expect_equal(t$Location[t$pitch_type == "CB"], "")
  t2 <- grades_table(o$grades, o$components, 102L, "MLB", 2026L, NULL, "global")
  expect_true(all(t2$chase == ""))
  expect_false(any(grepl("NaN|NA", unlist(t2[, c("Stuff", "Location", "Pitching", "chase", "whiff_swing")]))))
})

test_that("metric columns blank when display is NULL", {
  o <- fx()
  t <- grades_table(o$grades, o$components, 101L, "MLB", 2026L, NULL, "global")
  expect_true(all(is.na(t$velo))); expect_equal(nrow(t), 3)
})

test_that("pitch_summary filters metrics by batter side but keeps usage splits", {
  d <- fixture_display(); d <- d[d$pitcher == 101, ]
  s <- pitch_summary(d, "L")
  expect_equal(sum(s$n), sum(d$stand == "L"))
  expect_true(all(c("usage_l", "usage_r", "velo", "ivb", "hb", "spin", "ext", "vaa", "arm_angle", "rel_z", "rel_x") %in% names(s)))
})

test_that("single-pitch-type pitcher renders", {
  o <- fx()
  sel <- list(state = shiny::reactive(list(level = "MLB", season = 2026L, pitcher = 102L, side = "All", scale = "type", min_pitches = 0)))
  d <- fixture_display()
  display <- list(rows = shiny::reactive(d[d$pitcher == 102, ]))
  shiny::testServer(gradesServer, args = list(outputs = o, sel = sel, display = display), {
    expect_false(is.null(output$table))
    expect_match(output$header$html, "Traded Two")
  })
})

test_that("a pitcher without grades gets a message instead of an empty table", {
  o <- fx()
  sel <- list(state = shiny::reactive(list(level = "MLB", season = 2026L, pitcher = 999L, side = "All", scale = "type", min_pitches = 0)))
  display <- list(rows = shiny::reactive(NULL))
  shiny::testServer(gradesServer, args = list(outputs = o, sel = sel, display = display), {
    expect_match(output$empty$html, "No grades")
  })
})
