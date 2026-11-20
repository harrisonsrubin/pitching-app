# Helpers for the nightly ingest (scripts/ingest_daily.R).

# Re-check the last `window` days, not just yesterday, so a day Savant posted late
# (or a failed night) is picked up on a later run. Existing day files are skipped.
ingest_dates <- function(today, window = 5) seq(as.Date(today) - window, as.Date(today) - 1, by = "day")

# Open-Meteo's archive is built from reanalysis data and runs several days behind,
# so weather is ingested for a day `lag` days back rather than yesterday.
weather_date <- function(today, lag = 7) as.Date(today) - lag

weather_usable <- function(w, min_share = 0.9) nrow(w) > 0 && mean(!is.na(w$temp_c)) >= min_share

# Stats API files: a past season never changes once cached, but the current season
# (and the current year's transactions) must be refetched every time.
needs_fetch <- function(key, season, current) !store_exists(key) || season >= current
