# Historical weather at first pitch from Open-Meteo's archive API (free, no key).
# Requested in GMT and joined to games on the UTC hour containing first pitch.
# Indoor (roof-closed) handling happens later, when air density is computed (P1).

OPEN_METEO_ARCHIVE <- "https://archive-api.open-meteo.com/v1/archive"

open_meteo_request <- function(lat, lon, start_date, end_date) {
  list(url = OPEN_METEO_ARCHIVE,
       query = list(latitude = lat, longitude = lon,
                    start_date = format(as.Date(start_date)), end_date = format(as.Date(end_date)),
                    hourly = "temperature_2m,relative_humidity_2m,surface_pressure",
                    timezone = "GMT"))
}

num_or_na <- function(x) vapply(x, function(v) if (is.null(v)) NA_real_ else as.numeric(v), numeric(1))

parse_open_meteo <- function(json) {
  h <- json$hourly
  tibble::tibble(
    time_utc     = as.POSIXct(unlist(h$time), format = "%Y-%m-%dT%H:%M", tz = "UTC"),
    temp_c       = num_or_na(h$temperature_2m),
    rh_pct       = num_or_na(h$relative_humidity_2m),
    pressure_hpa = num_or_na(h$surface_pressure)
  )
}

fetch_hourly_weather <- function(lat, lon, start_date, end_date) {
  r <- open_meteo_request(lat, lon, start_date, end_date)
  parse_open_meteo(httr2::resp_body_json(http_get(r$url, r$query)))
}

join_game_weather <- function(games, hourly) {
  games %>%
    dplyr::mutate(time_utc = lubridate::floor_date(first_pitch_utc, "hour")) %>%
    dplyr::left_join(hourly, by = c("venue_id", "time_utc")) %>%
    dplyr::select(-time_utc)
}

game_weather_key <- function(level, season) sprintf("raw/weather/game_weather_%s_%s.parquet", level, season)

# One request per venue per season, covering that venue's first to last game.
build_game_weather <- function(schedule, parks) {
  venues <- schedule %>%
    dplyr::group_by(venue_id) %>%
    dplyr::summarise(start = min(game_date) - 1, end = max(game_date) + 1, .groups = "drop") %>%
    dplyr::inner_join(dplyr::select(parks, venue_id, lat, lon), by = "venue_id") %>%
    dplyr::filter(!is.na(lat), !is.na(lon))
  hourly <- dplyr::bind_rows(lapply(seq_len(nrow(venues)), function(i) {
    v <- venues[i, ]
    fetch_hourly_weather(v$lat, v$lon, v$start, v$end) %>% dplyr::mutate(venue_id = v$venue_id)
  }))
  join_game_weather(dplyr::select(schedule, game_pk, game_date, venue_id, first_pitch_utc), hourly)
}
