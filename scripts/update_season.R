# Usage: Rscript scripts/update_season.R <MLB|AAA> <season>
# Rebuilds one level-season end to end (P2 design 6-7): compact Statcast (current season)
# -> season weather -> prepare -> features -> score with the saved fold boosters ->
# grades on the frozen anchors -> components -> pitcher index -> display files.
# Run scripts/fetch_inputs.R first in CI. Exits non-zero on any failure.
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) stop("Usage: Rscript scripts/update_season.R <MLB|AAA> <season>")
lv <- args[1]; s <- as.integer(args[2])
step <- function(label, expr) {
  t0 <- Sys.time(); force(expr)
  message(sprintf("[%s %s] %s done in %.1f min", lv, s, label, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  invisible(gc())
}

if (has_day_files(lv, s)) step("compact", compact_season(lv, s))
step("weather", store_write(assemble_season_weather(lv, s), game_weather_key(lv, s)))
step("prepare", {
  p <- prepare_pitches(lv, s)
  store_write(p$pitches, prepared_key(lv, s)); store_write(p$offsets, park_offsets_key(lv, s))
  message(nrow(p$pitches), " pitches prepared"); rm(p)
})
refs <- read_refs()
step("features", { d <- build_features_season(lv, s, refs); store_write(d, features_key(lv, s)); rm(d) })
step("score", {
  d <- store_read(features_key(lv, s)); x <- score_pitches(d, refs)
  store_write(x, xrv_key(lv, s)); message(nrow(x), " pitches scored"); rm(d, x)
})
step("outputs", build_outputs(lv, s))
