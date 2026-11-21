p2b_fx <- function() { local_app_fixture(.frame = parent.frame()); load_outputs(app_config()) }

# ---- outputs ----
test_that("load_outputs brings in profiles, command, tunnel and anchors", {
  o <- p2b_fx()
  expect_true(all(c("profiles", "command", "tunnel", "anchors") %in% names(o)))
  expect_true(all(c("level", "season") %in% names(o$profiles)))
  expect_gt(nrow(o$anchors), 0)
})

# ---- Stuff Map ----
test_that("stuff map puts arm-side break on the positive side for both hands", {
  o <- p2b_fx()
  r <- stuff_map_data(o$profiles, o$grades, o$pitchers, "MLB", 2026L, "4S", 0)
  expect_true(all(r$arm_side[r$pitcher == 101] > 0))   # righty, catcher's-view hb negative
  s <- stuff_map_data(o$profiles, o$grades, o$pitchers, "MLB", 2026L, "SNK", 0)
  expect_true(all(s$arm_side > 0))                      # lefty, catcher's-view hb positive
  expect_true(all(c("name", "team", "grade", "ivb", "velo", "n") %in% names(r)))
})

test_that("stuff map filters by min pitches and handles a type nobody throws", {
  o <- p2b_fx()
  expect_equal(nrow(stuff_map_data(o$profiles, o$grades, o$pitchers, "MLB", 2026L, "4S", 10000)), 0)
  expect_equal(nrow(stuff_map_data(o$profiles, o$grades, o$pitchers, "MLB", 2026L, "KN", 0)), 0)
  sel <- list(state = shiny::reactive(list(level = "MLB", season = 2026L, pitcher = 101L, side = "All", scale = "type")))
  shiny::testServer(stuffmapServer, args = list(outputs = o, sel = sel), {
    session$setInputs(pitch_type = "KN", min_pitches = 0)
    expect_match(output$message$html, "No pitchers")
  })
})

# ---- Rolling ----
roll_rows <- function() {
  tibble::tibble(pitch_uid = sprintf("%d-%d-1", rep(1:30, each = 10), 1:300), game_pk = rep(1:30, each = 10),
                 game_date = as.Date("2026-04-01") + rep(0:29, each = 10), pitcher = 1L,
                 pitch_type = c(rep("4S", 290), rep("SL", 10)), velo = rep(c(95, 93), c(150, 150)),
                 stuff_xrv = rep(c(-0.01, 0.01), c(150, 150)), location_xrv = 0, pitching_xrv = 0)
}
anchors_fx <- function() tibble::tibble(facet = "stuff", scope = "type", pitch_type = c("4S", "SL"), mean = 0, unit = 0.005)

test_that("rolling uses the last 100 pitches of the type up to each game, from 50 pitches on", {
  r <- rolling_grades(roll_rows(), anchors_fx(), "stuff", window = 100, min_n = 50)
  expect_false("SL" %in% r$pitch_type)                     # only 10 SL pitches
  first <- r[r$pitch_type == "4S", ][1, ]
  expect_equal(first$n_window, 50)                          # game 5: 50 4S pitches so far
  g15 <- r[r$pitch_type == "4S" & r$game_date == as.Date("2026-04-15"), ]
  expect_equal(g15$n_window, 100)
  rr <- roll_rows(); s2 <- var(rr$stuff_xrv[rr$pitch_type == "4S"]); k <- 0.005^2 / (0.005^2 + s2 / 100)
  expect_equal(g15$value, 50 - 10 * k * (-0.01) / 0.005)   # all 100 at -0.01, pulled by the season noise
  g20 <- r[r$pitch_type == "4S" & r$game_date == as.Date("2026-04-20"), ]
  expect_lt(g20$value, 70)                                  # worse pitches entered the window
})

test_that("rolling velocity is a mean in mph", {
  r <- rolling_grades(roll_rows(), anchors_fx(), "velo", window = 100, min_n = 50)
  expect_equal(r$value[r$pitch_type == "4S"][1], 95)
})

# ---- Command & Tunneling ----
test_that("command table shows zone shares as percentages with the overall Location grade", {
  o <- p2b_fx()
  t <- command_table(o$command, o$grades, o$pitchers, "MLB", 2026L, "All", 0)
  expect_true(all(c("name", "team", "pitches", "zone", "heart", "shadow", "chase", "waste", "location") %in% names(t)))
  expect_true(all(abs(t$heart + t$shadow + t$chase + t$waste - 100) < 1e-6))
  expect_true(102L %in% command_table(o$command, o$grades, o$pitchers, "MLB", 2026L, "DET", 0)$pitcher)
})

test_that("tunnel pairs are unordered and unique, in inches", {
  o <- p2b_fx()
  p <- tunnel_pairs(o$tunnel, 103L, "MLB", 2026L)
  expect_equal(nrow(p), 6)                                  # 4S, CH, CB -> 3 pairs per batter side
  expect_false(any(duplicated(paste(p$vs, p$pair))))
  expect_equal(p$split_in, p$plate_gap_in - p$tunnel_gap_in)
  expect_equal(nrow(tunnel_pairs(o$tunnel, 102L, "MLB", 2026L)), 0)   # one pitch type
})

test_that("command page shows a message for a one-pitch pitcher's tunnels", {
  o <- p2b_fx()
  sel <- list(state = shiny::reactive(list(level = "MLB", season = 2026L, pitcher = 102L, side = "All", scale = "type")))
  shiny::testServer(commandServer, args = list(outputs = o, sel = sel), {
    session$setInputs(level = "MLB", season = "2026", team = "All", min_pitches = 0)
    expect_match(output$tunnel_message$html, "only one pitch type")
  })
})

test_that("tunnel pairs compare pitches within one batter side", {
  o <- p2b_fx()
  p <- tunnel_pairs(o$tunnel, 103L, "MLB", 2026L, side = "All")
  expect_true("vs" %in% names(p))
  expect_setequal(unique(p$vs), c("LHB", "RHB"))
  pr <- tunnel_pairs(o$tunnel, 103L, "MLB", 2026L, side = "R")
  expect_equal(unique(pr$vs), "RHB")
})

test_that("rolling windows are pulled toward the type average in proportion to their noise", {
  rr <- roll_rows()
  rr$stuff_xrv <- rep(c(-0.01, 0.01), 150)            # very noisy within the window, mean 0 -> about 50
  r <- rolling_grades(rr, anchors_fx(), "stuff", window = 100, min_n = 50)
  expect_true(all(abs(r$value[r$pitch_type == "4S"] - 50) < 1))
  quiet <- roll_rows(); quiet$stuff_xrv <- -0.01      # no noise -> no pull: 70 as before
  q <- rolling_grades(quiet, anchors_fx(), "stuff", window = 100, min_n = 50)
  expect_equal(q$value[q$pitch_type == "4S"][1], 70)
  noisy <- roll_rows(); set.seed(1); noisy$stuff_xrv <- -0.01 + rnorm(300, 0, 0.05)
  n <- rolling_grades(noisy, anchors_fx(), "stuff", window = 100, min_n = 50)
  expect_true(all(abs(n$value[n$pitch_type == "4S"] - 50) < abs(70 - 50)))   # pulled toward 50
})

test_that("rolling returns nothing (not an error) when the anchors are missing", {
  expect_equal(nrow(rolling_grades(roll_rows(), NULL, "stuff")), 0)
})
