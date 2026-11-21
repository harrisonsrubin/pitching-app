# Usage: Rscript scripts/freeze_projection_anchors.R
# Freezes the true talent grade's 20-80 anchors (P3a design): mean and SD of talent RA9
# among MLB pitchers with 50+ IP in 2023-2025, each season projected from data through
# that season only. Writes models/projection_anchors.csv. Run once; like the grade
# anchors, it stays fixed so grades are comparable across seasons.
source("R/load.R")
inp <- projection_inputs(through = 2025L)
pts <- dplyr::bind_rows(lapply(2023:2025, function(s) {
  i <- trim_inputs(inp, s)
  models <- fit_projection_models(i$lines, i$signals, NULL, i$parks, i$league, horizons = 1L)
  map <- fit_runs_map(i$lines, i$parks)
  dplyr::mutate(talent_point(models, i$lines, i$signals, i$parks, i$league, s, map), season = s)
}))
a <- projection_anchors(pts, inp$lines, 2023:2025)
readr::write_csv(a, file.path(repo_root(), "models", PROJECTION_ANCHORS_FILE))
print(a)
