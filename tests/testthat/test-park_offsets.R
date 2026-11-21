make_parks <- function(true_off) {
  set.seed(2)
  g <- expand.grid(pitcher = 1:60, venue_id = 1:3, rep = 1:40)
  g$pitch_type <- "4S"
  g$velo <- rnorm(60, 94, 2)[g$pitcher] + true_off[g$venue_id] + rnorm(nrow(g), 0, 0.5)
  tibble::as_tibble(g)
}

test_that("known offsets are recovered", {
  d <- make_parks(c(1, 0, -1))
  o <- fit_park_offsets(d, metrics = "velo", k = 0)
  expect_equal(o$offset[order(o$venue_id)], c(1, 0, -1), tolerance = 0.1)
})

test_that("a venue whose home pitchers are simply better gets no offset", {
  d <- make_parks(c(0, 0, 0))
  # venue 1's "home" pitchers throw 4 mph harder and pitch there far more often
  home <- d[d$pitcher <= 10, ]; home$venue_id <- 1L; home$velo <- home$velo + 4
  d$velo[d$pitcher <= 10] <- d$velo[d$pitcher <= 10] + 4
  o <- fit_park_offsets(dplyr::bind_rows(d, home, home), metrics = "velo", k = 0)
  expect_true(all(abs(o$offset) < 0.1))
})

test_that("apply subtracts and leaves unknown venues alone", {
  d <- tibble::tibble(venue_id = c(1L, 9L), velo = c(95, 95))
  o <- tibble::tibble(venue_id = 1L, metric = "velo", offset = 0.5, n = 100)
  expect_equal(apply_park_offsets(d, o)$velo, c(94.5, 95))
})

test_that("a camera offset is removed equally for lefties and righties (offsets fit in unmirrored space)", {
  set.seed(7)
  g <- expand.grid(pitcher = 1:40, venue_id = 1:3, rep = 1:30)
  g$p_throws <- ifelse(g$pitcher <= 25, "R", "L")
  talent <- rnorm(40, 10, 3)[g$pitcher]  # mirrored (arm-side) movement
  camera <- c(1, 0, -1)[g$venue_id]  # bias in raw catcher-view inches
  sign <- ifelse(g$p_throws == "L", -1, 1)
  g$hb <- sign * (sign * talent + camera)  # what prepare_mirror produces
  g$pitch_type <- "4S"
  d <- tibble::as_tibble(g)
  out <- apply_park_offsets(d, fit_park_offsets(d, metrics = "hb", k = 0))
  expect_lt(max(abs(out$hb - talent)), 0.1)
})
