plot_fx <- function() { local_app_fixture(.frame = parent.frame()); load_outputs(app_config()) }

test_that("movement plot has one trace per pitch type plus ellipses", {
  o <- plot_fx()
  d <- fixture_display(); d <- d[d$pitcher == 101, ]
  b <- plotly::plotly_build(movement_plot(d, o$movement_ref, o$pctl, "MLB", 2026L))$x$data
  names <- vapply(b, function(t) t$name %||% "", "")
  expect_true(all(c("4S", "SL") %in% names))
  expect_true(sum(grepl("league", names)) >= 1)
  expect_true(any(grepl("arm angle", names)))
})

test_that("hover carries the percentile and keys are pitch_uids", {
  o <- plot_fx()
  d <- fixture_display(); d <- d[d$pitcher == 101, ]
  b <- plotly::plotly_build(movement_plot(d, o$movement_ref, o$pctl, "MLB", 2026L))$x$data
  fb <- Filter(function(t) identical(t$name, "4S"), b)[[1]]
  expect_true(all(grepl("Stuff xRV/100 .* \\(pctl \\d+\\)", fb$text)))
  expect_setequal(unlist(fb$key), d$pitch_uid[d$pitch_type == "4S"])
})

test_that("corrected pitches get a black outline", {
  o <- plot_fx()
  d <- fixture_display(); d <- d[d$pitcher == 101, ]
  b <- plotly::plotly_build(movement_plot(d, o$movement_ref, o$pctl, "MLB", 2026L, corrected_uids = d$pitch_uid[1]))$x$data
  fb <- Filter(function(t) identical(t$name, d$pitch_type[1]), b)[[1]]
  w <- fb$marker$line$width
  expect_equal(sum(w > 0), 1)
})

test_that("location facets by side and type, tiny type falls back to points", {
  d <- fixture_display(); d <- d[d$pitcher == 103, ]
  d$pitch_type[1:3] <- "SP"
  p <- location_plot(d)
  lay <- ggplot2::ggplot_build(p)$layout$layout
  expect_setequal(unique(as.character(lay$stand)), c("vs LHB", "vs RHB"))
  expect_true(all(c("4S", "CH", "CB", "SP") %in% as.character(lay$pitch_type)))
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], "")
  expect_true("GeomPoint" %in% geoms)
  expect_true(any(grepl("Density|Polygon|Raster|Tile", geoms)))
})

test_that("NULL display shows the message", {
  o <- plot_fx()
  sel <- list(state = shiny::reactive(list(level = "MLB", season = 2026L, pitcher = 101L, side = "All", scale = "type", min_pitches = 0)))
  display <- list(rows = shiny::reactive(NULL), edits = shiny::reactiveVal(NULL))
  shiny::testServer(movementServer, args = list(outputs = o, sel = sel, display = display, cfg = app_config(), cache = bucket_cache()), {
    expect_match(output$message$html, "Pitch data unavailable")
  })
  shiny::testServer(locationServer, args = list(sel = sel, display = display), {
    expect_match(output$message$html, "Pitch data unavailable")
  })
})

test_that("movement plot is drawn from the pitcher's view (catcher's-view HB mirrored)", {
  o <- plot_fx()
  d <- fixture_display(); d <- d[d$pitcher == 101, ]
  b <- plotly::plotly_build(movement_plot(d, o$movement_ref, o$pctl, "MLB", 2026L))$x$data
  fb <- Filter(function(t) identical(t$name, "4S"), b)[[1]]
  expect_equal(as.numeric(unlist(fb$x)), -d$hb[d$pitch_type == "4S"])
  arm <- Filter(function(t) grepl("arm angle", t$name %||% ""), b)[[1]]
  expect_gt(unlist(arm$x)[2], 0)  # a righty's arm side is on his right
})
