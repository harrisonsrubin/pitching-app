# Usage: Rscript scripts/validate_data.R
# Validates every compacted Statcast season and writes data/reports/data_report.md
# (aggregate numbers only, no pitch rows).
source("R/load.R")
c0 <- cfg()
levels_seasons <- list(MLB = c0$seasons_mlb, AAA = c0$seasons_aaa)
res <- dplyr::bind_rows(lapply(names(levels_seasons), function(lv) {
  dplyr::bind_rows(lapply(levels_seasons[[lv]], function(s) {
    if (!store_exists(statcast_season_key(lv, s))) {
      return(tibble::tibble(level = lv, season = s, check = "season file exists", value = 0, pass = FALSE))
    }
    validate_statcast_season(lv, s)
  }))
}))

fmt <- function(v) ifelse(v > 1, format(round(v), big.mark = ","), sprintf("%.4f", v))
out <- file.path(c0$data_dir, "reports", "data_report.md")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
lines <- c("# Data report", "", sprintf("Generated %s", format(Sys.time(), "%Y-%m-%d %H:%M")), "",
           "| level | season | check | value | pass |", "|---|---|---|---|---|",
           sprintf("| %s | %s | %s | %s | %s |", res$level, res$season, res$check, fmt(res$value),
                   ifelse(is.na(res$pass), "n/a", ifelse(res$pass, "yes", "**NO**"))))
writeLines(lines, out)
failed <- res[!is.na(res$pass) & !res$pass, ]
message("Checks: ", nrow(res), "  failed: ", nrow(failed), "  report: ", out)
if (nrow(failed) > 0) {
  print(as.data.frame(failed))
  quit(status = 1)
}
