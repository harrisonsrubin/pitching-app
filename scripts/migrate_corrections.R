# One-time migration of the old app's pitch-type corrections.
# Usage:
#   Rscript scripts/migrate_corrections.R migrate <old_app_dir>
#       reads <old_app_dir>/pitch_corrections_<y>.csv and pitch_data_<y>_processed.csv,
#       writes corrections/pitch_corrections_<y>.csv (+ unmapped rows to data/reports/)
#   Rscript scripts/migrate_corrections.R verify
#       checks migrated corrections against backfilled Statcast seasons
source("R/load.R")
args <- commandArgs(trailingOnly = TRUE)
mode <- args[1]
seasons <- 2024:2026
report_dir <- file.path(cfg()$data_dir, "reports")
dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(repo_root(), "corrections"), showWarnings = FALSE)
report <- file.path(report_dir, "corrections_migration_report.md")

if (identical(mode, "migrate")) {
  old_dir <- args[2]
  lines <- c("# Corrections migration", "", "| season | old rows | unique play_ids | mapped | unmapped |", "|---|---|---|---|---|")
  for (y in seasons) {
    old <- read_old_corrections(file.path(old_dir, sprintf("pitch_corrections_%s.csv", y)))
    idx <- play_id_index(file.path(old_dir, sprintf("pitch_data_%s_processed.csv", y)))
    m <- migrate_corrections(old, idx)
    readr::write_csv(m$mapped, file.path(repo_root(), "corrections", sprintf("pitch_corrections_%s.csv", y)))
    readr::write_csv(m$unmapped, file.path(report_dir, sprintf("corrections_unmapped_%s.csv", y)))
    lines <- c(lines, sprintf("| %s | %s | %s | %s | %s |", y, nrow(old), dplyr::n_distinct(old$play_id),
                              nrow(m$mapped), nrow(m$unmapped)))
    message(y, ": old ", nrow(old), ", mapped ", nrow(m$mapped), ", unmapped ", nrow(m$unmapped))
  }
  writeLines(lines, report)
} else if (identical(mode, "verify")) {
  lines <- c("", "## Check against Savant pitch types", "",
             "| season | corrections | found in Statcast | original type matches Savant |", "|---|---|---|---|")
  for (y in seasons) {
    key <- statcast_season_key("MLB", y)
    if (!store_exists(key)) { message(y, ": Statcast not backfilled yet"); next }
    mapped <- readr::read_csv(file.path(repo_root(), "corrections", sprintf("pitch_corrections_%s.csv", y)),
                              col_types = readr::cols(.default = "c"))
    sc <- arrow::read_parquet(store_path(key), col_select = c("pitch_uid", "pitch_type"))
    v <- verify_against_statcast(mapped, sc)
    readr::write_csv(v$mismatches, file.path(report_dir, sprintf("corrections_mismatches_%s.csv", y)))
    lines <- c(lines, sprintf("| %s | %s | %s (%.1f%%) | %.1f%% |", y, nrow(mapped), v$n_found,
                              100 * v$n_found / max(nrow(mapped), 1), 100 * v$match_rate))
    message(y, ": found ", v$n_found, " of ", nrow(mapped), ", match rate ", round(v$match_rate, 3))
  }
  cat(lines, file = report, sep = "\n", append = TRUE)
} else {
  stop("Usage: Rscript scripts/migrate_corrections.R migrate <old_app_dir> | verify")
}
