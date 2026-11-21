test_that("parse_people reads name and throws", {
  j <- jsonlite::read_json(test_path("fixtures", "people.json"), simplifyVector = FALSE)
  p <- parse_people(j)
  expect_equal(p$player_id, c(669373L, 543037L))
  expect_equal(p$name, c("Tarik Skubal", "Gerrit Cole"))
  expect_equal(p$throws, c("L", "R"))
})

idx_pitches <- function() {
  tibble::tibble(
    pitcher = c(1L, 1L, 1L, 2L),
    game_pk = c(10L, 10L, 20L, 30L),
    game_date = as.Date(c("2026-04-01", "2026-04-01", "2026-07-01", "2026-05-01")),
    home_team = c("DET", "DET", "BOS", "SEA"),
    away_team = c("CLE", "CLE", "NYY", "TEX"),
    inning_topbot = c("Top", "Top", "Bot", "Bot"),
    p_throws = c("L", "L", "L", "R"))
}

test_that("traded pitcher lists both teams, latest first as his team", {
  stats <- tibble::tibble(player_id = 1L, age = 27L, g = 2L, gs = 2L, ip_outs = 30L)
  people <- tibble::tibble(player_id = c(1L, 2L), name = c("A", "B"), throws = c("L", "R"))
  out <- pitcher_index(idx_pitches(), stats, people, level = "MLB", season = 2026L)
  a <- out[out$pitcher == 1L, ]
  expect_equal(a$team_abbrev, "NYY")
  expect_equal(a$teams, "DET,NYY")
  expect_equal(a$pitches, 3L)
  expect_equal(a$name, "A")
})

test_that("pitcher without stats row keeps NA stats", {
  stats <- tibble::tibble(player_id = 1L, age = 27L, g = 2L, gs = 2L, ip_outs = 30L)
  people <- tibble::tibble(player_id = 1L, name = "A", throws = "L")
  out <- pitcher_index(idx_pitches(), stats, people, level = "MLB", season = 2026L)
  b <- out[out$pitcher == 2L, ]
  expect_true(is.na(b$ip_outs)); expect_true(is.na(b$name))
  expect_equal(b$throws, "R")  # falls back to Statcast's p_throws
  expect_equal(b$team_abbrev, "TEX")
  expect_named(out, c("level", "season", "pitcher", "name", "throws", "team_abbrev", "teams",
                      "pitches", "g", "gs", "ip_outs", "age"))
})

test_that("outputs_key prefixes outputs/", {
  expect_equal(outputs_key("pitchers_MLB_2026.parquet"), "outputs/pitchers_MLB_2026.parquet")
})
