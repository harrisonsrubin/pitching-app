# Usage: Rscript scripts/build_features.R <MLB|AAA> <first_season> <last_season>
# Prepared pitches + models/ reference tables -> every model feature, the denoised
# target, and outcome flags, in processed/features/<level>/<season>.parquet.
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) stop("Usage: Rscript scripts/build_features.R <MLB|AAA> <first> <last>")
refs <- read_refs()
for (s in seq(as.integer(args[2]), as.integer(args[3]))) {
  d <- build_features_season(args[1], s, refs)
  store_write(d, features_key(args[1], s))
  message(args[1], " ", s, ": ", nrow(d), " pitches with features")
  rm(d); gc()
}
