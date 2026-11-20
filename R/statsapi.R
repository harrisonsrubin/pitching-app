# MLB Stats API: schedules (with venue and game weather), venues, season pitching
# stats, and transactions. Each fetcher is a thin wrapper around a pure parse_*()
# function so the parsers can be tested on synthetic JSON.

STATSAPI <- "https://statsapi.mlb.com/api/v1"

statsapi_json <- function(path, query = list()) {
  resp <- http_get(paste0(STATSAPI, path), query)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

`%|||%` <- function(x, y) if (is.null(x)) y else x

# ── Schedules ─────────────────────────────────────────────────────────────────
# Finished games only: codedGameState "F" (final, including completed early) or
# "O" (game over). Postponed ("D") and suspended games are dropped; a suspended game
# reappears as final on the date it is resumed, and the later entry is kept.
FINISHED_STATES <- c("F", "O")

parse_schedule <- function(json, level) {
  games <- unlist(lapply(json$dates, function(d) d$games), recursive = FALSE)
  if (length(games) == 0) {
    return(tibble::tibble(game_pk = integer(), game_date = as.Date(character()),
                          first_pitch_utc = as.POSIXct(character(), tz = "UTC"),
                          venue_id = integer(), level = character(), status = character(),
                          double_header = character(), weather_condition = character(),
                          weather_temp_f = numeric(), weather_wind = character()))
  }
  rows <- lapply(games, function(g) {
    w <- g$weather %|||% list()
    tibble::tibble(
      game_pk           = as.integer(g$gamePk),
      game_date         = as.Date(g$officialDate),
      first_pitch_utc   = as.POSIXct(g$gameDate, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      venue_id          = as.integer(g$venue$id %|||% NA),
      level             = level,
      status            = g$status$codedGameState %|||% NA_character_,
      double_header     = g$doubleHeader %|||% NA_character_,
      weather_condition = w$condition %|||% NA_character_,
      weather_temp_f    = suppressWarnings(as.numeric(w$temp %|||% NA)),
      weather_wind      = w$wind %|||% NA_character_
    )
  })
  dplyr::bind_rows(rows) %>%
    dplyr::filter(status %in% FINISHED_STATES) %>%
    dplyr::arrange(dplyr::desc(game_date)) %>%
    dplyr::distinct(game_pk, .keep_all = TRUE) %>%
    dplyr::arrange(game_date, game_pk)
}

schedule_key <- function(season, level) sprintf("raw/statsapi/schedule/%s/%s.parquet", level, season)

fetch_schedule <- function(season, level) {
  key <- schedule_key(season, level)
  if (store_exists(key)) return(store_read(key))
  json <- statsapi_json("/schedule", list(sportId = cfg()$sport_ids[[level]], season = season,
                                          gameType = "R", hydrate = "weather,venue"))
  s <- parse_schedule(json, level)
  # Only cache seasons that are over; the current season keeps changing.
  if (season < as.integer(format(Sys.Date(), "%Y")) || format(Sys.Date(), "%m") %in% c("11", "12")) {
    store_write(s, key)
  }
  s
}

game_days <- function(season, level) sort(unique(fetch_schedule(season, level)$game_date))

# ── Venues and the park table ─────────────────────────────────────────────────
parse_venues <- function(json) {
  dplyr::bind_rows(lapply(json$venues, function(v) {
    loc <- v$location %|||% list()
    co  <- loc$defaultCoordinates %|||% list()
    tibble::tibble(
      venue_id     = as.integer(v$id),
      venue_name   = v$name %|||% NA_character_,
      lat          = as.numeric(co$latitude %|||% NA),
      lon          = as.numeric(co$longitude %|||% NA),
      elevation_ft = as.numeric(loc$elevation %|||% NA),
      roof_type    = (v$fieldInfo %|||% list())$roofType %|||% NA_character_,
      azimuth_deg  = as.numeric(loc$azimuthAngle %|||% NA)
    )
  }))
}

fetch_venues <- function(ids) {
  chunks <- split(ids, ceiling(seq_along(ids) / 50))
  dplyr::bind_rows(lapply(chunks, function(ch) {
    parse_venues(statsapi_json("/venues", list(venueIds = paste(ch, collapse = ","),
                                               hydrate = "location,fieldInfo")))
  }))
}


# ── People ────────────────────────────────────────────────────────────────────
parse_people <- function(json) {
  p <- json$people %|||% list()
  tibble::tibble(
    player_id = vapply(p, function(x) as.integer(x$id), integer(1)),
    name      = vapply(p, function(x) x$fullName %|||% NA_character_, character(1)),
    throws    = vapply(p, function(x) x$pitchHand$code %|||% NA_character_, character(1)))
}

fetch_people <- function(ids, chunk = 150) {
  ids <- unique(stats::na.omit(as.integer(ids)))
  if (length(ids) == 0) return(parse_people(list()))
  groups <- split(ids, ceiling(seq_along(ids) / chunk))
  dplyr::bind_rows(lapply(groups, function(g)
    parse_people(statsapi_json("/people", list(personIds = paste(g, collapse = ","))))))
}
