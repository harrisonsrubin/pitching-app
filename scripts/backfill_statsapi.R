# Usage: Rscript scripts/backfill_statsapi.R [schedules] [parks] [stats] [transactions] [current]
# With no arguments, runs schedules, parks, stats, transactions. Cached past seasons
# are skipped; the current season and year are always refetched.
# "current" refetches only the current season's stats and the current year's transactions.
source("R/load.R")
stages <- commandArgs(trailingOnly = TRUE)
if (length(stages) == 0) stages <- c("schedules", "parks", "stats", "transactions")
c0 <- cfg()
current <- as.integer(format(Sys.Date(), "%Y"))
if ("current" %in% stages) {
  stages <- c(stages, "stats", "transactions")
  c0$seasons_stats <- current
  c0$seasons_aaa <- current
}
levels_seasons <- list(MLB = c0$seasons_mlb, AAA = c0$seasons_aaa)

if ("schedules" %in% stages || "parks" %in% stages) {
  schedules <- dplyr::bind_rows(lapply(names(levels_seasons), function(lv) {
    dplyr::bind_rows(lapply(levels_seasons[[lv]], function(s) {
      message("Schedule ", lv, " ", s)
      fetch_schedule(s, lv)
    }))
  }))
  message("Finished games: ", nrow(schedules))
}

if ("parks" %in% stages) {
  venues <- fetch_venues(sort(unique(schedules$venue_id)))
  parks <- build_parks(schedules, venues, dplyr::select(read_park_overrides(), -dplyr::any_of("note"))) %>%
    fill_missing_elevation()
  readr::write_csv(parks, file.path(repo_root(), "reference", "parks.csv"), na = "")
  message("Wrote reference/parks.csv: ", nrow(parks), " venues")
}

if ("stats" %in% stages) {
  stat_seasons <- list(MLB = c0$seasons_stats, AAA = c0$seasons_aaa)
  for (lv in names(stat_seasons)) for (s in stat_seasons[[lv]]) {
    key <- sprintf("raw/statsapi/pitching/%s/%s.parquet", lv, s)
    if (!needs_fetch(key, s, current)) next
    message("Pitching stats ", lv, " ", s)
    store_write(fetch_pitching_stats(s, lv), key)
  }
}

if ("transactions" %in% stages) {
  for (y in c0$seasons_stats) {
    key <- sprintf("raw/statsapi/transactions/%s.parquet", y)
    if (!needs_fetch(key, y, current)) next
    message("Transactions ", y)
    store_write(fetch_transactions(as.Date(sprintf("%s-01-01", y)), as.Date(sprintf("%s-12-31", y))), key)
  }
}
