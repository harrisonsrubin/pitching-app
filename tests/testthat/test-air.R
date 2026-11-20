test_that("reference air is about 1.194 kg/m3", {
  expect_equal(RHO_REF, air_density(21.111, 50, 1013.25))
  expect_equal(RHO_REF, 1.194, tolerance = 0.002)
})

test_that("Coors on a standard day is about 17% thinner", {
  r <- air_density(21.111, 50, pressure_from_elevation(5190))
  expect_true(r > 0.97 && r < 1.00)
})

test_that("hot and humid is thinner than cold and dry", {
  expect_lt(air_density(35, 90, 1013), air_density(5, 20, 1013))
})

test_that("domes ignore outdoor heat; missing weather falls back to elevation", {
  sched <- tibble::tibble(game_pk = 1:3, venue_id = c(10L, 20L, 20L),
                          weather_condition = c("Dome", "Sunny", "Sunny"))
  wx <- tibble::tibble(game_pk = 1:3, temp_c = c(35, 35, NA), rh_pct = c(20, 20, NA),
                       pressure_hpa = c(1000, 1000, NA))
  parks <- tibble::tibble(venue_id = c(10L, 20L), elevation_ft = c(0, 0), roof_type = c("Retractable", "Open"))
  a <- game_air(sched, wx, parks)
  expect_equal(a$air_source, c("indoor", "weather", "elevation_only"))
  expect_equal(a$rho[1], air_density(21.111, 50, 1000))
  expect_equal(a$rho[3], RHO_REF, tolerance = 1e-9)
  expect_false(any(is.na(a$rho)))
})
