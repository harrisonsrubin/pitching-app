# Usage: Rscript scripts/score_pitches.R <MLB|AAA> <first_season> <last_season>
# Scores every pitch with the pitcher-held-out fold models -> processed/xrv/<level>/<season>.parquet
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) stop("Usage: Rscript scripts/score_pitches.R <MLB|AAA> <first> <last>")
refs <- read_refs()
for (s in seq(as.integer(args[2]), as.integer(args[3]))) {
  d <- store_read(features_key(args[1], s))
  x <- score_pitches(d, refs)
  store_write(x, xrv_key(args[1], s))
  message(args[1], " ", s, ": scored ", nrow(x), "; mean stuff_xrv ", round(mean(x$stuff_xrv, na.rm = TRUE), 5),
          " vs mean target ", round(mean(x$target, na.rm = TRUE), 5))
  rm(d, x); gc()
}
