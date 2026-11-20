test_that("missing elevations are filled from the lookup in feet; known ones are kept", {
  parks <- tibble::tibble(venue_id = 1:3, lat = c(39.7, 35.0, NA), lon = c(-105, -106, NA),
                          elevation_ft = c(5190, NA, NA))
  lookup <- function(lat, lon) { expect_equal(lat, 35.0); 1000 }   # meters
  out <- fill_missing_elevation(parks, lookup = lookup)
  expect_equal(out$elevation_ft[1], 5190)
  expect_equal(round(out$elevation_ft[2]), 3281)
  expect_true(is.na(out$elevation_ft[3]))
  expect_equal(out$elevation_source, c("statsapi", "open-meteo", NA))
})

test_that("open-meteo elevation request uses comma-joined coordinates", {
  r <- open_meteo_elevation_request(c(1.5, 2.5), c(-3, -4))
  expect_equal(r$query$latitude, "1.5,2.5")
  expect_equal(r$query$longitude, "-3,-4")
})

test_that("an existing elevation_source (e.g. a hand override) is kept", {
  parks <- tibble::tibble(venue_id = 1L, lat = 19.4, lon = -99.1, elevation_ft = 7349,
                          elevation_source = "override")
  expect_equal(fill_missing_elevation(parks, lookup = function(...) stop("not called"))$elevation_source,
               "override")
})
