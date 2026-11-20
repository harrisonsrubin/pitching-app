# Usage: Rscript scripts/build_features.R <MLB|AAA> <first_season> <last_season>
# Prepared pitches + models/ reference tables -> every model feature, the denoised
# target, and outcome flags, in processed/features/<level>/<season>.parquet.
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) stop("Usage: Rscript scripts/build_features.R <MLB|AAA> <first> <last>")
refs <- read_refs()
for (s in seq(as.integer(args[2]), as.integer(args[3]))) {
  d <- store_read(prepared_key(args[1], s))
  d <- add_stuff_features(d, refs)
  d <- harmonize_zone(d, refs$zone_map, is_abs_era(args[1], s))   # see R/zone.R
  d <- add_location_features(d)
  d <- add_sequence_features(d)
  d$target <- denoised_target(d, refs$bip)
  d <- dplyr::select(d, -dplyr::any_of(c("vx0", "vy0", "vz0", "ax", "ay", "az")))
  store_write(d, features_key(args[1], s))
  message(args[1], " ", s, ": ", nrow(d), " pitches with features")
  rm(d); gc()
}
