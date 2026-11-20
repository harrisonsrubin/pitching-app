# Usage: Rscript scripts/build_grades.R
# 1. Anchors from MLB reference seasons (cfg()$seasons_ref) -> models/grade_anchors.csv
# 2. For every level-season: grades (full season; MLB also odd/even game halves for
#    split-half reliability) -> grades/<level>/<season>.parquet, and expected component
#    rates -> components/<level>/<season>.parquet. Triple-A uses the MLB anchors.
source("R/load.R")
FACETS <- c("stuff", "location", "pitching")
c0 <- cfg()

ref_fits <- list()
for (s in c0$seasons_ref) {
  x <- store_read(xrv_key("MLB", s))
  for (f in FACETS) ref_fits[[f]][[as.character(s)]] <- fit_grades(x, paste0(f, "_xrv"))
  message("anchor fits: MLB ", s)
}
anchors <- dplyr::bind_rows(lapply(FACETS, function(f) grade_anchors(ref_fits[[f]], f)))
readr::write_csv(anchors, file.path(repo_root(), "models", "grade_anchors.csv"))

levels_seasons <- list(MLB = c0$seasons_mlb, AAA = c0$seasons_aaa)
for (lv in names(levels_seasons)) for (s in levels_seasons[[lv]]) {
  x <- store_read(xrv_key(lv, s))
  fits <- if (lv == "MLB" && s %in% c0$seasons_ref) lapply(ref_fits, `[[`, as.character(s)) else NULL
  g <- grade_season(x, lv, s, anchors, fits)
  store_write(g$grades, grades_key(lv, s))
  store_write(g$components, components_key(lv, s))
  message(lv, " ", s, ": ", sum(g$grades$half == "full" & g$grades$pitch_type == "ALL") / length(FACETS), " pitchers graded")
  rm(x, g); gc()
}
