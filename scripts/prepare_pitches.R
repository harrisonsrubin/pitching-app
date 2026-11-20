# Usage: Rscript scripts/prepare_pitches.R <MLB|AAA> <first_season> <last_season>
# Writes processed/prepared/<level>/<season>.parquet and the fitted park offsets.
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) stop("Usage: Rscript scripts/prepare_pitches.R <MLB|AAA> <first> <last>")
for (s in seq(as.integer(args[2]), as.integer(args[3]))) {
  p <- prepare_pitches(args[1], s)
  store_write(p$pitches, prepared_key(args[1], s))
  store_write(p$offsets, park_offsets_key(args[1], s))
  message(args[1], " ", s, ": ", nrow(p$pitches), " pitches prepared")
  rm(p); gc()
}
