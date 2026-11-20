# Usage: Rscript scripts/ingest_daily.R [YYYY-MM-DD]
# Nightly ingest for MLB and AAA. "Today" defaults to the current date in
# America/New_York. Re-checks the last 5 days of Statcast (each day checked against
# the schedule; existing complete days are skipped) and ingests weather for the day
# 7 days back, because Open-Meteo's archive runs several days behind.
# Each level is independent: a failure in one does not skip the other.
# Exits non-zero if anything failed.
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
today <- if (length(args) >= 1) as.Date(args[1]) else as.Date(format(Sys.time(), tz = "America/New_York", "%Y-%m-%d"))
parks <- readr::read_csv(file.path(repo_root(), "reference", "parks.csv"), show_col_types = FALSE)
failed <- FALSE

ingest_level <- function(lv) {
  dates <- ingest_dates(today)
  seasons <- unique(as.integer(format(dates, "%Y")))
  sched_days <- function(s, l) {
    d <- game_days(s, l)
    d[d %in% dates]
  }
  log <- backfill_statcast(lv, seasons, days = sched_days, today = today)
  if (nrow(log) > 0) message(lv, ": ", paste(log$date, log$status, collapse = "; "))
  bad <- backfill_failed(log)

  wday <- weather_date(today)
  wkey <- sprintf("raw/weather/daily/%s/%s.parquet", lv, format(wday))
  games <- dplyr::filter(fetch_schedule(as.integer(format(wday, "%Y")), lv), game_date == wday)
  if (nrow(games) > 0 && !store_exists(wkey)) {
    w <- build_game_weather(games, parks)
    if (weather_usable(w)) store_write(w, wkey) else {
      message(lv, " weather for ", wday, ": not available yet (will retry next run)")
    }
  }
  bad
}

for (lv in names(cfg()$sport_ids)) {
  res <- tryCatch(ingest_level(lv), error = function(e) {
    message(lv, ": FAILED - ", conditionMessage(e))
    TRUE
  })
  failed <- failed || isTRUE(res)
}
if (failed) quit(status = 1)
