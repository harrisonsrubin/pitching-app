# Usage: Rscript scripts/backfill_weather.R
# Builds raw/weather/game_weather_<level>_<season>.parquet for every backfilled
# schedule (one Open-Meteo request per venue per season). Existing files are skipped.
source("R/load.R")
c0 <- cfg()
parks <- readr::read_csv(file.path(repo_root(), "reference", "parks.csv"), show_col_types = FALSE)
levels_seasons <- list(MLB = c0$seasons_mlb, AAA = c0$seasons_aaa)
for (lv in names(levels_seasons)) for (s in levels_seasons[[lv]]) {
  key <- game_weather_key(lv, s)
  if (store_exists(key)) next
  message("Weather ", lv, " ", s)
  w <- build_game_weather(fetch_schedule(s, lv), parks)
  message("  games: ", nrow(w), "  missing temp: ", sum(is.na(w$temp_c)))
  store_write(w, key)
}
