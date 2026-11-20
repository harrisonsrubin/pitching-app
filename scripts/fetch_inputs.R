# Usage: Rscript scripts/fetch_inputs.R <MLB|AAA> <season>
# Downloads from the private store everything scripts/update_season.R needs for one
# level-season: Statcast (the daily files for the current season, the compacted season
# file otherwise), game weather, schedule, Stats API pitching, and the scoring boosters.
# Needs env PV_DATA_REPO and GH_TOKEN.
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop("Usage: Rscript scripts/fetch_inputs.R <MLB|AAA> <season>")
lv <- args[1]; s <- as.integer(args[2])
repo <- Sys.getenv("PV_DATA_REPO")
if (!nzchar(repo)) stop("Set PV_DATA_REPO to the private data repo (owner/name).")

current <- is_current_season(s)
if (current) {
  release_download_dir(sprintf("raw/statcast/%s/%s", lv, s), repo)
  release_download_dir(sprintf("raw/weather/daily/%s", lv), repo)
} else {
  release_download_file(statcast_season_key(lv, s), repo)
}
if (current || !store_exists(game_weather_key(lv, s))) {
  ok <- tryCatch({ release_download_file(game_weather_key(lv, s), repo); TRUE }, error = function(e) FALSE)
  if (!ok) message("No season weather file for ", lv, " ", s, " yet; daily files only")
}
ensure_schedule(lv, s, repo, current)
# Season stats feed only the pitcher index (G, GS, IP); a missing file leaves them blank.
stats_key <- sprintf("raw/statsapi/pitching/%s/%s.parquet", lv, s)
if (!store_exists(stats_key))
  tryCatch(release_download_file(stats_key, repo), error = function(e) message("No stored stats for ", lv, " ", s))
for (name in names(MODEL_SPECS)) release_download_dir(sprintf("models/lgb/%s", name), repo)
message("Inputs ready for ", lv, " ", s, if (current) " (current season)")
