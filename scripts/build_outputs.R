# Usage: Rscript scripts/build_outputs.R <MLB|AAA> <first_season> <last_season>
# Grades (frozen anchors), components, pitcher index and display files from the stored
# features + xRV, without re-preparing. Used once to fill past seasons for the app.
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) stop("Usage: Rscript scripts/build_outputs.R <MLB|AAA> <first> <last>")
anchors <- read_anchors()
for (s in seq(as.integer(args[2]), as.integer(args[3]))) { build_outputs(args[1], s, anchors); gc() }
