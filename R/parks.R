# Park reference table: one row per venue used by any MLB or AAA regular-season game
# in the backfill seasons. Venue facts come from the Stats API; hand fixes in
# reference/parks_overrides.csv are applied last.

build_parks <- function(schedules, venues, overrides = NULL) {
  used <- schedules %>%
    dplyr::filter(!is.na(venue_id)) %>%
    dplyr::group_by(venue_id) %>%
    dplyr::summarise(
      level = if (dplyr::n_distinct(level) > 1) "both" else dplyr::first(level),
      first_season = as.integer(min(format(game_date, "%Y"))),
      last_season  = as.integer(max(format(game_date, "%Y"))),
      .groups = "drop"
    )
  parks <- dplyr::left_join(used, venues, by = "venue_id")

  if (!is.null(overrides) && nrow(overrides) > 0) {
    for (col in setdiff(names(overrides), "venue_id")) {
      idx <- match(parks$venue_id, overrides$venue_id)
      has <- !is.na(idx) & !is.na(overrides[[col]][idx])
      parks[[col]][has] <- overrides[[col]][idx[has]]
    }
  }
  dplyr::arrange(parks, venue_id)
}

read_park_overrides <- function() {
  p <- file.path(repo_root(), "reference", "parks_overrides.csv")
  if (!file.exists(p)) return(NULL)
  readr::read_csv(p, show_col_types = FALSE, progress = FALSE)
}

# ── Elevation fill ────────────────────────────────────────────────────────────
# The Stats API has no elevation for many AAA parks. Open-Meteo's free elevation
# API (90 m terrain model) fills the gaps; it agrees with the Stats API on Coors Field.
open_meteo_elevation_request <- function(lat, lon) {
  list(url = "https://api.open-meteo.com/v1/elevation",
       query = list(latitude = paste(lat, collapse = ","), longitude = paste(lon, collapse = ",")))
}

open_meteo_elevation <- function(lat, lon) {
  r <- open_meteo_elevation_request(lat, lon)
  unlist(httr2::resp_body_json(http_get(r$url, r$query))$elevation)
}

fill_missing_elevation <- function(parks, lookup = open_meteo_elevation) {
  default_src <- ifelse(is.na(parks$elevation_ft), NA_character_, "statsapi")
  parks$elevation_source <- if ("elevation_source" %in% names(parks)) dplyr::coalesce(parks$elevation_source, default_src) else default_src
  need <- which(is.na(parks$elevation_ft) & !is.na(parks$lat) & !is.na(parks$lon))
  if (length(need) > 0) {
    meters <- lookup(parks$lat[need], parks$lon[need])
    parks$elevation_ft[need] <- meters * 3.28084
    parks$elevation_source[need] <- "open-meteo"
  }
  parks
}
